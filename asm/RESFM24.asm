*          DATA SET RESFM24    AT LEVEL 057 AS OF 05/01/02                      
*PHASE T81824A                                                                  
*INCLUDE DAYVAL                                                                 
*INCLUDE RETIMVAL                                                               
         TITLE 'T81824 - RESFM24 - DIRECT RESPONSE RECORD'                      
***********************************************************************         
*                                                                     *         
*  RESFM24 (T81824) --- MAINTENANCE/LIST OF DIRECT RESPONSE CARDS     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01SEP92 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 20MAY93 (SKU) ADD COPY ACTION                                       *         
*                                                                     *         
* 28FEB94 (SKU) FOR ACTION LIST, FILTER ON PERIOD THEN GROUP (IF      *         
*               SELECTED) INTERNALLY SO THE NUMBER OF IO'S ARE REDUCED*         
*                                                                     *         
* 08OCT06 (SEP) ALLOW LOW POWER TV STATIONS                           *         
*                                                                     *         
* 28JAN97 (SKU) BUG FIX                                               *         
*                                                                     *         
* 19NOV97 (JRD) YR2000 PWOS DATE                                      *         
***********************************************************************         
T81824   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81824*,R7,RR=R3                                              
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
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         XC    ACURSOR,ACURSOR                                                  
         XC    DRSTAT,DRSTAT       STATUS BITS                                  
*                                                                               
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
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
K        USING RDIRKEY,KEY         REMOVE HARD KEY LENGTHS                      
KS       USING RDIRKEY,KEYSAVE     REMOVE HARD KEY LENGTHS                      
SV       USING RDIRKEY,SVKEY       REMOVE HARD KEY LENGTHS                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   KEYCHGED,C'N'       ASSUME KEY IS UNCHANGED                      
         TM    DIRSTATH+4,X'20'    TEST STATION WAS CHANGED                     
         BZ    VK50                                                             
         TM    DIRPERIH+4,X'20'    TEST PERIOD WAS CHANGED                      
         BO    VK100                                                            
*                                                                               
VK50     DS    0H                                                               
         MVI   KEYCHGED,C'Y'       YES, KEY HAS CHANGED                         
         OC    KEY,KEY                                                          
         BNZ   VK100                                                            
         XC    CPYFRKEY,CPYFRKEY                                                
         B     VK105                                                            
*                                                                               
VK100    DS    0H                                                               
         MVC   CPYFRKEY,KEY        SAVE PREVIOUS KEY                            
VK105    XC    KEY,KEY                                                          
         MVI   K.RDIRTYP,RDIRTYPQ                                               
         MVC   K.RDIRREP,AGENCY                                                 
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK130                                                            
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,DSLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   DSLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK110                                                            
*                                                                               
         CLI   DSLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
*                                                                               
         OC    DSLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R5,KEY                                                           
         USING RGRPKEY,R5                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,DSLGRUP                                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RGRPKEY),KEYSAVE                                           
         BNE   INVLGRP                                                          
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     VK120                                                            
*                                                                               
VK110    DS    0H                  FILTER ON STATION ?                          
         CLI   DSLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         LA    R2,DSLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
*                                                                               
VK120    DS    0H                  FILTER ON PERIOD?                            
         CLI   DSLPERIH+5,0                                                     
         BE    VKX                                                              
         LA    R2,DSLPERIH                                                      
         BAS   RE,VALIPERI                                                      
         B     VKX                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION CALL LETTERS                                                 
***********************************************************************         
VK130    DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,DIRSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   K.RDIRSTA,WORK      VALISTA PUTS OUTPUT IN WORK                  
         MVC   EDRSTAT,DIRSTAT     SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SV.RDIRSTA                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DIRLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   VK140                                                            
*                                                                               
         MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   VK140                                                            
         MVC   DIRLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
VK140    OI    DIRLOCAH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVKEY                                                        
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
***********************************************************************         
VK150    DS    0H                                                               
         MVC   SVKEY,KEY           VALIPER USES KEY                             
         LA    R2,DIRPERIH         VALIDATE PERIOD                              
         BAS   RE,VALIPERI                                                      
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   K.RDIRENDT,ENDDT                                                 
         MVC   K.RDIRSTDT,STARTDT                                               
         MVC   EDRPERI,DIRPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
VKX      DS    0H                                                               
         OI    DIRSTATH+4,X'20'    STATION HAS BEEN VALIDATED                   
         OI    DIRPERIH+4,X'20'    PERIOD HAS BEEN VALIDATED                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   VR00                                                             
         GOTO1 =A(COPY),DMCB,(RC),RR=Y                                          
         B     EXIT                                                             
*                                                                               
VR00     DS    0H                                                               
         LA    R2,DIRCTIDH         CONTROL ID MUST BE ENTERED                   
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,RDIRDCDQ     REMOVE OLD DESCRIPTIVE ELEMENT               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,RDIRPCDQ     REMOVE OLD PROGRAM DAY/TIME ELEM             
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RDIRDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RDIRDCDE,RDIRDCDQ   ELEMENT CODE                                 
         MVI   RDIRDELN,RDIRDOV    OVERHEAD LENGTH                              
         MVC   RDIRDCID,DIRCTID    CONTROL ID                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RDIRDLUP)  LAST UPDATE DATE                 
*                                                                               
         LA    R2,DIRDESCH         DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDIRDESC(0),DIRDESC                                              
*                                                                               
         ZIC   RF,RDIRDELN         ADD VARIABLE LENGTH FROM                     
         LA    RF,1(R1,RF)         DESCRIPTION FIELD                            
         STC   RF,RDIRDELN                                                      
*                                                                               
VR15     DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR20     DS    0H                                                               
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
         LA    R2,DIRHEADH         1ST FIELD WHICH COULD CONTAIN CURSOR         
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
         LA    RE,L'LENFLD                                                      
VR50     ZIC   RF,0(R2)            BUMP TO FIRST FIELD OF NEXT LINE             
         AR    R2,RF                                                            
         BCT   RE,VR50                                                          
*                                                                               
         LA    RF,DIRLDAYH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR200               YES                                          
         B     VR40                                                             
*                                                                               
VR60     CLI   PFKEY,ADDLINE       JUMP TO ROUTINE                              
         BE    VR120                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE LINE                                                                   
***********************************************************************         
         LA    R0,DIRHEADH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
*                                                                               
         LA    RE,L'LENFLD                                                      
VR70     ZIC   R0,0(R3)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         BCT   RE,VR70                                                          
*                                                                               
VR80     LA    RF,DIRLDAYH         A(LAST TEXT FIELD)                           
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR100               YES                                          
*                                                                               
         LA    RE,LENFLD           MOVE ALL FIELDS FROM LOWER LINE              
         LA    RF,L'LENFLD         TO ONE BEFORE                                
VR90     ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         ZIC   R0,0(R3)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR90                                                          
         B     VR80                                                             
*                                                                               
VR100    LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR110    XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELDS                                 
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR110                                                         
         B     VR200                                                            
         EJECT                                                                  
***********************************************************************         
* ADD LINE                                                                      
***********************************************************************         
VR120    DS    0H                  ARE THEY TRYING TO INSERT AFTER END?         
         LA    RF,DIRLDAYH                                                      
         CR    R2,RF                                                            
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         ST    R2,AINSERT          SAVE A(INSERTION)                            
         LA    R3,DIRLDAYH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR130    DS    0H                                                               
         LA    R0,DIRDAY2H         COMPUTE LENGTH OF ONE LINE                   
         LA    R1,DIRDAYH                                                       
         SR    R0,R1                                                            
         SR    R2,R0               R2 POINTS TO PREVIOUS LINE                   
*                                                                               
         L     RF,AINSERT                                                       
         CR    R2,RF               ARE WE AT THE CURSOR?                        
         BE    VR150               YES                                          
*                                                                               
         ST    R2,SAVER2                                                        
*                                                                               
         LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR140    DS    0H                                                               
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         ZIC   R0,0(R3)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR140                                                         
*                                                                               
         L     R2,SAVER2                                                        
         LR    R3,R2                                                            
         B     VR130                                                            
*                                                                               
VR150    LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR160    XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR FIELDS                                 
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR160                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ADD/UPDATE PROGRAM DAY/TIME ELEMENT                                           
***********************************************************************         
VR200    DS    0H                                                               
         LA    R2,DIRDAYH                                                       
         MVI   SEQNUM,1                                                         
*                                                                               
VR210    ST    R2,SAVER2                                                        
         LA    RF,L'LENFLD         CHECK ENTIRE LINE FOR ANY INPUT              
*                                                                               
         BAS   RE,VALDYTM          VALIDATE DAY AND TIME                        
*                                                                               
VR220    CLI   5(R2),0                                                          
         BNE   VR230                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   RF,VR220                                                         
         B     VR250               BLANK LINE                                   
*                                                                               
VR230    DS    0H                                                               
         OI    DRSTAT,NOBLNKLN     YES, WE HAVE AT LEAST ONE LINE               
         L     R2,SAVER2                                                        
         LA    R6,ELEM                                                          
         USING RDIRPRGD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RDIRPCDE,RDIRPCDQ   ELEMENT CODE                                 
         MVI   RDIRPELN,RDIRPELQ   ELEMENT LENGTH                               
         MVC   RDIRPSQ,SEQNUM      SEQUENCE NUMBER                              
         LA    R6,RDIRPDAY                                                      
         DROP  R6                                                               
*                                                                               
         LA    R3,LENFLD           MOVE LINE OF SCREEN FIELDS TO                
         LA    RF,L'LENFLD         FIELDS IN ELEMENT                            
VR240    DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         CLI   5(R2),0             SKIP IF NOTHING                              
         BE    VR245                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),8(R2)                                                    
*                                                                               
VR245    DS    0H                                                               
         LA    R6,1(R1,R6)         NEXT ELEMENT FIELD                           
         ZIC   R1,0(R2)            NEXT SCREEN FIELD                            
         AR    R2,R1                                                            
         LA    R3,1(R3)            NEXT SCREEN FIELD LENGTH                     
         BCT   RF,VR240                                                         
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR250    ZIC   RF,SEQNUM                                                        
         LA    RF,1(RF)            INCREMENT SEQUENCE NUMBER                    
         STC   RF,SEQNUM                                                        
*                                                                               
         LA    RF,DIRLDAYH                                                      
         CR    R2,RF                                                            
         BNH   VR210                                                            
*                                                                               
         TM    DRSTAT,NOBLNKLN                                                  
         BNZ   VRX                                                              
         LA    R2,DIRDAYH                                                       
         B     MISSFLD                                                          
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAY AND TIME FIELDS (TOOK THE ROUTINE FROM RECNT15)                  
***********************************************************************         
VALDYTM  NTR1                                                                   
*                                                                               
* PREPARE TO EDIT STRING OF DAY-TIME FIELDS IN PARALLEL                         
*                                                                               
         LR    R3,R2                                                            
         LA    R4,7(R3)            DIRDAY+7                                     
         LR    R5,R3               DIRDAYH                                      
         ST    R3,ADAY                                                          
*                                                                               
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         LA    R6,7(R3)            DIRTIM+7                                     
         LR    R8,R3               DIRTIMH                                      
         ST    R3,ATIME                                                         
*                                                                               
         L     RF,ADAY                                                          
         CLI   5(RF),0             IF NO INPUT FOR BOTH DAY AND TIME            
         BNE   VALDT10             EXIT                                         
         L     RF,ATIME                                                         
         CLI   5(RF),0                                                          
         BE    VALDTX                                                           
*                                                                               
VALDT10  DS     0H                                                              
         STM   R4,R6,DMCB+8        PARAMETERS FOR SCAN                          
         ST    R8,DMCB+20                                                       
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R8,R8               END DAY                                      
*                                                                               
VALDT20  DS     0H                                                              
         L     R2,ADAY                                                          
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         XC    WORK+2(6),WORK+2                                                 
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   VALDT40                                                          
*              NO DAY LENGTH                                                    
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   VALDT30                                                          
         B     INVLDAY                                                          
*                                                                               
VALDT30  CLI    DMCB+20,C'*'        ANOTHER TIME ENTRY?                         
         BNE   VALDTX                                                           
         B     INVLDAY                                                          
*                                                                               
VALDT40  MVC    DMCB2(4),DMCB+8      DAY FIELD ADDR + LEN                       
*                                                                               
* EDIT DAY FIELD                                                                
*                                                                               
         GOTO1 =V(DAYVAL),DMCB2,,WORK+3,WORK+2,RR=YES                           
*                                                                               
         CLI   WORK+3,0            VALID DAY?                                   
         BNE   VALDT50                                                          
         B     INVLDAY                                                          
*                                                                               
VALDT50  LA     R6,1(R6)            COUNTER                                     
*                                                                               
* GET FIRST START DAY AND LAST END DAY                                          
*                                                                               
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK+2           START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    VALDT60                                                          
* MAKE SURE NO MORE THAN 7 DAYS COVERED                                         
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    VALDT60                                                          
         B     INVLDAY             MORE THAN 7 DAYS                             
VALDT60  CR    R5,R8               END                                          
         BNH   *+6                                                              
         LR    R8,R5               NEW HIGH END                                 
*                                                                               
* EDIT TIME FIELD                                                               
*                                                                               
         L     R2,ATIME                                                         
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   VALDT70                                                          
         B     INVLTIM                                                          
*                                                                               
VALDT70  ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   VALDT90                                                          
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   VALDT80                                                          
         B     INVLTIM                                                          
VALDT80  MVC   WORK+4,=C'VARY'                                                  
         B     VALDT130                                                         
*                                                                               
VALDT90  EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   VALDT110            OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   VALDT100                                                         
         B     INVLTIM                                                          
*                                                                               
VALDT100 MVC   WORK+4,=C'NONE'                                                  
         B     VALDT130                                                         
*                                                                               
VALDT110 MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
         GOTO1 =V(RETIMVAL),DMCB,,WORK+4,RR=YES   EDIT TIME                     
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   VALDT120                                                         
         B     INVLTIM                                                          
*                                                                               
VALDT120 OC    WORK+6(2),WORK+6    IS THERE AN END TIME?                        
         BZ    VALDT130            NO                                           
         CLC   WORK+4(2),=H'0500' START TIME LT 5AM?                            
         BNL   VALDT130            NO                                           
         CLC   WORK+6(2),=H'0500' END TIME GT = 5AM?                            
         BL    VALDT130            NO                                           
         B     INVLBDAY            END TIME MUST BE W/IN B'CAST DAY             
*              ADD DAY-TIME ELEMENT TO BUYREC                                   
VALDT130 DS    0H                                                               
         B     VALDT20             EDIT NEXT DAY TIME FIELD COMBO               
*                                                                               
VALDTX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   DIRSTAT(4),RDIRSTA                                               
         MVC   DIRSTAT+4(2),=C'-L'                                              
         CLI   RDIRSTA+4,C'L'                                                   
         BE    DK05                                                             
         MVC   DIRSTAT+4(2),=C'-T'                                              
         CLI   RDIRSTA+4,C' '                                                   
         BZ    DK05                                                             
         MVC   DIRSTAT+5(1),RDIRSTA+4                                           
DK05     OI    DIRSTATH+6,X'80'    XMIT                                         
         MVC   EDRSTAT,DIRSTAT     SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,DIRPERI)                                 
*                                                                               
         MVI   DIRPERI+8,C'-'                                                   
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,DIRPERI+9)                               
         OI    DIRPERIH+6,X'80'    XMIT                                         
         MVC   EDRPERI,DIRPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SV.RDIRSTA                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DIRLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   DK20                                                             
*                                                                               
DK10     DS    0H                                                               
         MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   DK20                                                             
         MVC   DIRLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
DK20     OI    DIRLOCAH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC DIRCTIDH            CLEAR SCREEN                                 
         L     R6,AIO                                                           
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR05                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,RDIRDLUP),(5,DIRLDAT)                             
         OI    DIRLDATH+6,X'80'    XMIT                                         
*                                                                               
         MVC   DIRCTID,RDIRDCID                                                 
         OI    DIRCTIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   RDIRDELN,RDIRDOV    ANY DESCRIPTION?                             
         BNH   DR05                                                             
*                                                                               
         ZIC   R1,RDIRDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RDIRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DIRDESC(0),RDIRDESC                                              
         OI    DIRDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         L     R6,AIO                                                           
         USING RDIRPRGD,R6                                                      
         MVI   ELCODE,RDIRPCDQ     PROGRAM DAY/TIME ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         SR    R5,R5               LINE COUNTER                                 
         LA    R2,DIRDAYH          FIRST FIELD                                  
*                                                                               
DR10     LA    R3,RDIRPDAY         POINT TO RECORD                              
         LA    RE,LENFLD           COPY FROM RECORD SCREEN                      
         LA    RF,L'LENFLD                                                      
         LA    R5,1(R5)            INCREMENT LINE COUNTER                       
*                                                                               
DR20     DS    0H                                                               
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
*                                                                               
         ZIC   R4,RDIRPSQ          SEQUENCE NUMBER                              
         CR    R4,R5               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR30                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
*                                                                               
DR30     OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R3,1(R1,R3)                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,DR20                                                          
*                                                                               
         LA    RF,DIRLDAYH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
*                                                                               
         ZIC   R4,RDIRPSQ          SEQUENCE NUMBER                              
         CR    R4,R5               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR10                                                             
*                                                                               
         BAS   RE,NEXTEL           NEXT LINE OF TEXT                            
         BE    DR10                                                             
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SAVEKEY         USE SELECTED KEY                             
         XC    SAVEKEY,SAVEKEY                                                  
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'RDIRKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
*                                                                               
         CLI   DSLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RDIRSTA,STATION     YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                  MATCHING REC TYPE AND REP CODE               
         CLC   KEY(RDIRSTA-RDIRKEY),KEYSAVE                                     
         BNE   LRX                                                              
*                                                                               
         CLI   DSLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   K.RDIRSTA,KS.RDIRSTA                                             
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
LR30     CLI   K.RDIRMAST,0        MUST BE MASTER RECORD                        
         BNE   LRSEQ                                                            
*                                                                               
         CLI   DSLPERIH+5,0        FILTER ON PERIOD?                            
         BE    LR35                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         CLC   STARTDT,K.RDIRENDT  THESE DATES ARE IN 9'S COMP!                 
         BL    LRSEQ                                                            
         CLC   ENDDT,K.RDIRSTDT                                                 
         BH    LRSEQ                                                            
*                                                                               
LR35     DS    0H                                                               
         CLI   DSLGRUPH+5,0        FILTER ON GROUP?                             
         BE    LR60                                                             
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SV.RDIRSTA                                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     STATION RECORD MUST BE THERE                 
         BE    LR40                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     LRSEQ                                                            
*                                                                               
LR40     MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R1,DSLGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),DSLGRUP                                              
         BE    LR50                                                             
         DROP  R6                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     LRSEQ                                                            
*                                                                               
LR50     DS    0H                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
*                                                                               
LR60     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LDRSTAT(4),RDIRSTA  STATION CALL LETTER                          
         MVC   LDRSTAT+4(2),=C'-T'                                              
         CLI   RDIRSTA+4,C' '                                                   
         BE    LR75                                                             
         MVC   LDRSTAT+4(2),=C'-L'                                              
         CLI   RDIRSTA+4,C'L'                                                   
         BE    LR75                                                             
         MVC   LDRSTAT+5(1),RDIRSTA+4                                           
*                                  PERIOD                                       
LR75     DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,LDRPERI)                                 
         MVI   LDRPERI+8,C'-'                                                   
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,LDRPERI+9)                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              DESCRIPTION                                  
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR100               SKIP IF NO ELEMENT                           
*                                                                               
         CLI   RDIRDELN,RDIRDOV                                                 
         BNH   LR90                                                             
*                                                                               
         ZIC   R1,RDIRDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RDIRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         CH    R1,=H'19'           MAX LEN IS 20                                
         BNH   LR80                                                             
         LA    R1,19                                                            
*                                                                               
LR80     DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDRDESC(0),RDIRDESC                                              
*                                                                               
LR90     DS    0H                                                               
         MVC   LDRCTID,RDIRDCID    CONTROL ID AND LAST UPDATE DATE              
         GOTO1 DATCON,DMCB,(3,RDIRDLUP),(5,LDRLUPD)                             
         DROP  R6                                                               
*                                                                               
LR100    DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
***********************************************************************         
SCAN     NTR1                                                                   
         L     R2,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R2)       TOTAL LENGTH OF INPUT                             
         LA    R2,8(R4,R2)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R2          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         B     EXIT                                                             
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+12                                                             
         MVI   4(R1),C'*'                                                       
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT - R2 POINTS TO PERIOD FIELD HEADER                                      
*         KEY IS SETUP WITH COPY-TO DR STATION                                  
* OUTPUT - STARTDT HAS START DATE                                               
*          ENDDT HAS END DATE                                                   
***********************************************************************         
VALIPERI NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLPER             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,12(R5),WORK                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
         GOTO1 DATCON,DMCB,WORK,(19,STARTDT)     START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,22(R5),WORK+6                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
*                                                                               
         CLC   WORK+6(6),WORK      END V START DATE                             
         BL    INVLDAT             ERR - END DATE BEFORE START DATE             
*                                                                               
         GOTO1 DATCON,DMCB,WORK+6,(19,ENDDT)     END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   ENDDT,WORK                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   STARTDT,WORK                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VALPER05                                                         
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   VALPERX                                                          
*                                                                               
* IF ACTION ADD, CHECK FOR OVERLAPPING PERIOD                                   
*                                                                               
VALPER05 DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 HIGH                                                             
*                                                                               
VALPER10 CLC   KEY(20),KEYSAVE     OK IF DIFFERENT STATION                      
         BNE   VALPERX                                                          
         CLI   K.RDIRMAST,0        MUST BE MASTER REC                           
         BNE   VALPER30                                                         
         CLC   STARTDT,K.RDIRSTDT  CAN'T OVERLAP                                
         BE    INVLDOVL                                                         
         CLC   ENDDT,K.RDIRENDT                                                 
         BE    INVLDOVL                                                         
*                                                                               
         CLC   ENDDT,K.RDIRENDT    IS NEW END DATE EARLIER THAN                 
         BH    VALPER20            EXISTING END DATE?                           
*                                                                               
         CLC   STARTDT,K.RDIRENDT  NO, NEW START DATE HAS TO BE                 
         BL    VALPER30            LATER THAN EXISTING END DATE                 
         B     INVLDOVL                                                         
*                                                                               
VALPER20 CLC   ENDDT,K.RDIRSTDT    YES, NEW END DATE HAS TO BE                  
         BNH   INVLDOVL            EARLIER THAN EXISTING START DATE             
*                                                                               
VALPER30 DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 SEQ                                                              
         B     VALPER10                                                         
*                                                                               
VALPERX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COPY DIRECT REPONSE AND CORRESPONDING NOTES RECORDS                           
***********************************************************************         
COPY     DS    0H                                                               
         OC    CPYFRKEY,CPYFRKEY   FIRST PASS, ASK FOR COPY TO KEY              
         BZ    NOCPYKEY                                                         
         CLI   KEYCHGED,C'N'                                                    
         BE    NOTOKEY                                                          
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY,CPYFRKEY        RETRIEVE COPY FROM KEY                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   FROMNOTF            COPY FROM KEY NOT FOUND                      
*                                                                               
         GOTO1 GETREC              DR RECORD                                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK NEW KEY AGAINST EXISTING RECORDS FOR OVERLAPPING PERIOD                 
*                                                                               
         XC    KEY,KEY             CHECK IF NEW KEY EXISTS                      
         MVC   KEY(20),SVKEY       DO HIGH ON STATION IN VALIPERI               
         LA    R2,DIRPERIH                                                      
         BAS   RE,VALIPERI                                                      
*                                                                               
         XC    KEY,KEY             CHECK IF NEW NOTES KEY EXISTS                
         MVC   KEY,SVKEY                                                        
         MVI   K.RDIRMAST,X'01'    DR NOTES RECORD                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   *+6                 ERROR, NEW NOTES KEY EXISTS                  
         DC    H'0'                THIS IS SERIOUS, SO DUMP                     
*                                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         MVC   RDIRKEY,SVKEY       NEW STATION AND/OR PERIOD                    
*                                                                               
         ZIC   R1,RDIRDELN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RDIRDESD    MAKE A COPY OF THE DESC. ELEMENT             
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,RDIRDCDQ     DESCRIPTIVE ELEMENT                          
         GOTO1 REMELEM             NOW, DELETE IT                               
*                                                                               
         LA    R6,ELEM             AND BUILD A NEW ONE                          
         USING RDIRDESD,R6                                                      
         CLI   DIRCTIDH+5,0        IF USER PUT IN NEW CTRL ID                   
         BE    COPY10              COPY TO NEW                                  
         MVC   RDIRDCID,DIRCTID    CONTROL ID                                   
*                                                                               
COPY10   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,RDIRDLUP)  LAST UPDATE DATE                 
*                                                                               
         LA    R2,DIRDESCH         DESCRIPTION                                  
         CLI   5(R2),0             IF USER PUT IN  NEW DESCRIPTION              
         BE    COPY20              COPY IT OVER                                 
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDIRDESC(0),DIRDESC                                              
*                                                                               
         LA    RF,RDIRDOV          ADD VARIABLE LENGTH WITH OVERHEAD            
         LA    RF,1(R1,RF)         TO DESCRIPTION FIELD                         
         STC   RF,RDIRDELN                                                      
         DROP  R6                                                               
*                                                                               
COPY20   DS    0H                  ADD NEW DR RECORD                            
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVC   RDIRKEY,CPYFRKEY    RETRIEVE COPY FROM KEY                       
         MVI   RDIRMAST,X'01'      DR NOTES RECORD                              
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       IF DR NOTES RECORD EXISTS,                   
         GOTO1 HIGH                COPY IT, TOO                                 
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   COPYX                                                            
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         MVC   RDIRKEY,SVKEY       NEW STATION AND/OR PERIOD                    
         MVI   RDIRMAST,X'01'      DR NOTES RECORD                              
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDREC              ADD NEW DR NOTES RECORD                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPYX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLDOVL MVC   RERROR,=AL2(INVDOV)                                              
         B     ERREND                                                           
*                                                                               
INVLFILT MVC   RERROR,=AL2(INVFIL)                                              
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(INVGRP)                                              
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(INVPER)                                              
         B     ERREND                                                           
*                                                                               
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
*                                                                               
INVLDAY  MVC   RERROR,=AL2(DAYERR)                                              
         B     ERREND                                                           
*                                                                               
INVLTIM  MVC   RERROR,=AL2(TIMERR)                                              
         B     ERREND                                                           
*                                                                               
INVLBDAY MVC   RERROR,=AL2(INVBDAY)                                             
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
NOCPYKEY MVC   RERROR,=AL2(380)                                                 
         B     ERREND                                                           
*                                                                               
NOTOKEY  MVC   RERROR,=AL2(381)                                                 
         B     ERREND                                                           
*                                                                               
NEWKEYEX MVC   RERROR,=AL2(382)                                                 
         B     ERREND                                                           
*                                                                               
FROMNOTF MVC   RERROR,=AL2(382)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
* TABLE FOR LENGTH OF FIELDS IN ONE PROGRAM DAY/TIME LINE                       
*                                                                               
LENFLD   DS    0XL7                                                             
         DC    AL1(L'DIRDAY),AL1(L'DIRTIME),AL1(L'DIRPROG)                      
         DC    AL1(L'DIRT30),AL1(L'DIRT60),AL1(L'DIRT90)                        
         DC    AL1(L'DIRT120)                                                   
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
ACTCOPY  EQU   15                                                               
ADDLINE  EQU   5                                                                
DELLINE  EQU   6                                                                
RELO     DS    A                                                                
DMCB2    DS    6F                  FOR DAY/TIME VALIDATION                      
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
SAVEKEY  DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
AINSERT  DS    F                   A(INSERTION)                                 
ACURSOR  DS    A                   FORCE CURSOR HERE                            
ADAY     DS    A                   A(DAY) HEADER FIELD                          
ATIME    DS    A                   A(TIME) HEADER FIELD                         
SEQNUM   DS    X                                                                
STATION  DS    CL5                 STATION CALL LETTERS                         
DRSTAT   DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
PFKEYUSE EQU   X'04'               PFKEY WAS USED                               
KEYCHGED DS    C                   KEY HAS CHANGED Y/N                          
CPYFRKEY DS    CL(L'KEY)           KEY COPYING FROM                             
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC5D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC6D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENDIR                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENGRP                                                       
         PRINT ON                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LDRSTAT  DS    CL7                                                              
         DS    CL2                                                              
LDRDESC  DS    CL20                                                             
         DS    CL2                                                              
LDRPERI  DS    CL17                                                             
         DS    CL2                                                              
LDRCTID  DS    CL8                                                              
         DS    CL2                                                              
LDRLUPD  DS    CL8                                                              
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057RESFM24   05/01/02'                                      
         END                                                                    
