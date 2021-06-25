*          DATA SET RESFM26    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T81826A                                                                  
         TITLE 'T81826 - RESFM26 - DIRECT RESPONSE NOTE'                        
***********************************************************************         
*                                                                     *         
*  RESFM26 (T81826) --- NOTE INPUT OF DIRECT RESPONSE CARDS           *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01SEP92 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 04JUN93 (SKU) KEEP CURSOR IN PLACE WHEN INSERTING LINES             *         
*               PROTECT CTID AND DESCRIPTION FIELD                    *         
*                                                                     *         
* 28FEB94 (SKU) FILTER ON PERIOD FIRST THEN GROUP INTERNALLY TO       *         
*               REDUCE THE NUMBER OF IO'S                             *         
*                                                                     *         
* 08JUN95 (SKU) CHANGE CONTROL ID TO NOT REQUIRED INPUT FIELD         *         
*                                                                     *         
* 19NOV97 (JRD) YR2000 PWOS DATE                                      *         
***********************************************************************         
T81826   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81826*,R7,RR=R3                                              
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
         MVI   IOOPT,C'Y'          DO MY OWN I/O 'S                             
*                                                                               
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
K        USING RDIRKEY,KEY         REMOVE HARD KEY LENGTHS                      
KS       USING RDIRKEY,KEYSAVE     REMOVE HARD KEY LENGTHS                      
SV       USING RDIRKEY,SVKEY       REMOVE HARD KEY LENGTHS                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RDIRKEY,R4                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVI   RDIRMAST,1          NOTES SCREEN RECORD                          
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK30                                                             
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,DNLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   DNLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK10                                                             
*                                                                               
         CLI   DNLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
*                                                                               
         OC    DNLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R5,KEY                                                           
         USING RGRPKEY,R5                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,DNLGRUP                                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RGRPKEY),KEYSAVE                                           
         BNE   INVLGRP                                                          
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     VK20                                                             
*                                                                               
VK10     DS    0H                  FILTER ON STATION ?                          
         CLI   DNLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         LA    R2,DNLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
*                                                                               
VK20     DS    0H                  FILTER ON PERIOD?                            
         CLI   DNLPERIH+5,0                                                     
         BE    VKX                                                              
         LA    R2,DNLPERIH                                                      
         BAS   RE,VALIPERI                                                      
         B     VKX                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK30     DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,DRNSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   RDIRSTA,WORK        SAVE CALL LETTERS                            
         MVC   EDRSTAT,DRNSTAT     SAVE IN CASE WE PF TO MASTER SCRN            
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
         MVC   DRNLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   VK35                                                             
*                                                                               
         MVC   AIO,AIO2            USE IO2                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   VK35                                                             
         MVC   DRNLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
VK35     OI    DRNLOCAH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,DRNPERIH         VALIDATE PERIOD                              
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
         GOTO1 DATCON,DMCB,WORK+6,(19,ENDDT)     END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   RDIRENDT,WORK                                                    
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   RDIRSTDT,WORK                                                    
         MVC   EDRPERI,DRNPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VK40                                                             
*                                                                               
         CLI   ACTNUM,ACTREST                                                   
         BE    VKX                                                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE     RECORD NOT FOUND                      
         BE    VKX                                                              
         LA    R2,CONACTH                                                       
         B     RECNOTF                                                          
*                                                                               
* FOR ACTION ADD, CHECK IF MASTER RECORD EXISTS                                 
*                                                                               
VK40     DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2                                                         
         XC    RDIRMAST,RDIRMAST                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE     MASTER RECORD MUST BE THERE           
         BNE   INVLMAST                                                         
*                                                                               
         CLI   DRNCTIDH+5,0                                                     
         BNE   VK50                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
*                                                                               
         GOTO1 DATCON,DMCB,(3,RDIRDLUP),(5,DRNLDAT)                             
         OI    DRNLDATH+6,X'80'    XMIT                                         
*                                                                               
         MVC   DRNCTID,RDIRDCID                                                 
         MVI   DRNCTIDH+5,L'DRNCTID                                             
         OI    DRNCTIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   RDIRDELN,RDIRDOV    ANY DESCRIPTION?                             
         BNH   VK50                                                             
*                                                                               
         ZIC   R1,RDIRDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RDIRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
*                                                                               
         STC   R1,DRNDESCH+5       INSERT LENGTH                                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRNDESC(0),RDIRDESC                                              
         OI    DRNDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
VK50     MVC   KEY,SVKEY           RESTORE KEY/AIO                              
         MVC   AIO,AIO1                                                         
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE THE MASTER RECORD                                                      
***********************************************************************         
XRP      DS    0H                                                               
         MVC   KEY,XSVKEY                                                       
         LA    R6,KEY                                                           
         USING RDIRREC,R6                                                       
         XC    RDIRMAST,RDIRMAST   UPDATE DESCRIPTIVE ELEMENT IN MASTER         
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   INVLMAST            MASTER RECORD NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         GOTO1 DATCON,DMCB,(5,0),(3,RDIRDLUP)  LAST UPDATE DATE                 
         DROP  R6                                                               
*                                                                               
XRP10    DS    0H                                                               
         GOTO1 PUTREC              UPDATE MASTER                                
         MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
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
         LA    R2,DRNHEADH         1ST FIELD WHICH COULD CONTAIN CURSOR         
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
         LA    RF,DRNLTXTH                                                      
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
         LA    R0,DRNHEADH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR130    DS    0H                                                               
         LA    RF,DRNLTXTH                                                      
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR140               YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'DRNTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR130                                                            
*                                                                               
VR140    XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'DRNTXT                                                      
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
         LA    RF,DRNLTXTH                                                      
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,DRNLTXTH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR160    ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR170               YES                                          
         LA    R1,L'DRNTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR160                                                            
*                                                                               
VR170    XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'DRNTXT                                                      
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
*                                                                               
         L     R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RDIRNCDQ     NOTES ELEMENT CODE                           
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR210    DS    0H                                                               
         NI    DRSTAT,X'FF'-NOBLNKLN   FLAG FOR INPUT                           
         LA    R2,DRNTXTH          FIRST TEXT FIELD                             
         MVI   SEQNUM,0                                                         
*                                                                               
VR220    LA    R6,ELEM                                                          
         USING RDIRNTED,R6                                                      
*                                                                               
         LA    RF,DRNLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR300               YES                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   RDIRNCDE,RDIRNCDQ   NOTE TEXT LINE ELEMENT CODE                  
         MVC   RDIRNTSQ,SEQNUM     SEQUENCE NUMBER                              
*                                                                               
         CLC   =C'C=',8(R2)                                                     
         BNE   VR225                                                            
         MVC   SVKEY,KEY           VALICMT USES KEY                             
         BAS   RE,VALICMT          VALIDATE FILE COMMENT RECORD                 
         MVC   KEY,SVKEY                                                        
*                                                                               
VR225    DS    0H                                                               
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR230                                                            
         OI    DRSTAT,NOBLNKLN     SOME TEXT WAS FOUND                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDIRNOTE(0),8(R2)   NOTE TEXT LINE                               
         LA    R3,RDIRNTOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,1(R3,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR250                                                            
*                                                                               
VR230    ST    R2,SAVER2           HANG ON TO CURRENT TWA POINTER               
*                                                                               
VR240    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         LA    RF,DRNLTXTH                                                      
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
VR250    STC   R1,RDIRNELN                                                      
*                                                                               
VR260    CLI   5(R2),0             DON'T ADD BLANK LINE                         
         BE    VR270                                                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR270    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         B     VR220                                                            
*                                                                               
VR300    DS    0H                                                               
         TM    DRSTAT,NOBLNKLN                                                  
         BNZ   VR305                                                            
         LA    R2,DRNTXTH          FIRST TEXT FIELD                             
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
         MVC   XSVKEY,KEY                                                       
         B     DR                                                               
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
         MVC   DRNSTAT(4),RDIRSTA                                               
         MVC   DRNSTAT+4(2),=C'-T'                                              
         CLI   RDIRSTA+4,C' '                                                   
         BZ    DK05                                                             
         MVC   DRNSTAT+5(1),RDIRSTA+4                                           
DK05     OI    DRNSTATH+6,X'80'    XMIT                                         
         MVC   EDRSTAT,DRNSTAT     SAVE IN CASE WE PF TO MASTER SCRN            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,DRNPERI)                                 
*                                                                               
         MVI   DRNPERI+8,C'-'                                                   
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,DRNPERI+9)                               
         OI    DRNPERIH+6,X'80'    XMIT                                         
         MVC   EDRPERI,DRNPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
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
         MVC   DRNLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   DK20                                                             
*                                                                               
DK10     DS    0H                                                               
         MVC   AIO,AIO2            USE IO2                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   DK20                                                             
         MVC   DRNLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
DK20     OI    DRNLOCAH+6,X'80'    XMIT                                         
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
         TWAXC DRNCTIDH            CLEAR SCREEN                                 
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         XC    RDIRMAST,RDIRMAST                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   INVLMAST            CAN'T FIND MASTER RECORD                     
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
*                                                                               
         GOTO1 DATCON,DMCB,(3,RDIRDLUP),(5,DRNLDAT)                             
         OI    DRNLDATH+6,X'80'    XMIT                                         
*                                                                               
         MVC   DRNCTID,RDIRDCID                                                 
         OI    DRNCTIDH+6,X'80'    XMIT                                         
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
         MVC   DRNDESC(0),RDIRDESC                                              
         OI    DRNDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RDIRNTED,R6                                                      
         MVI   ELCODE,RDIRNCDQ     NOTES ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         SR    R5,R5               LINE COUNTER                                 
         LA    R2,DRNTXTH          FIRST NOTE FIELD                             
*                                                                               
DR10     MVC   8(L'DRNTXT,R2),SPACES                                            
         LA    R5,1(R5)            INCREMENT LINE COUNTER                       
         ZIC   R1,RDIRNTSQ         SEQUENCE NUMBER                              
         CR    R1,R5               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR15                                                             
*                                                                               
         ZIC   R1,RDIRNELN         LENGTH OF ELEMENT                            
         LA    R3,RDIRNTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RDIRNOTE    LINE OF TEXT                                 
*                                                                               
         CLC   =C'C=',8(R2)        DISPLAY FILE COMMENT, IF ANY                 
         BNE   DR15                                                             
         MVC   SVKEY,KEY                                                        
         BAS   RE,DISCMT                                                        
         MVC   KEY,SVKEY                                                        
*                                                                               
DR15     OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,DRNLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
*                                                                               
         ZIC   R1,RDIRNTSQ         SEQUENCE NUMBER                              
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
         LA    RF,DRNLTXTH                                                      
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
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         OI    RDIRCNTL,X'80'      MARK FOR DELETION                            
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
***********************************************************************         
* RESTORE THE RECORD                                                            
***********************************************************************         
RESR     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   INVLFLD             NOTHING TO RESTORE                           
*                                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       READ FOR DELETE                              
         GOTO1 GETREC                                                           
*                                                                               
         NI    RDIRCNTL,X'FF'-X'80'  RESTORE                                    
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
***********************************************************************         
* LIST THE RECORD                                                               
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
         CLI   DNLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RDIRSTA,STATION     YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(RDIRSTA-RDIRKEY),KEYSAVE                                     
         BNE   LRX                                                              
*                                                                               
         CLI   DNLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   K.RDIRSTA,KS.RDIRSTA                                             
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
LR30     CLI   K.RDIRMAST,1        MUST BE NOTES RECORD                         
         BNE   LRSEQ                                                            
*                                                                               
         CLI   DNLPERIH+5,0        FILTER ON PERIOD?                            
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
         CLI   DNLGRUPH+5,0        FILTER ON GROUP?                             
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
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R1,DNLGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),DNLGRUP                                              
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
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LDRSTAT(4),RDIRSTA STATION CALL LETTER                           
         MVC   LDRSTAT+4(2),=C'-T'                                              
         CLI   RDIRSTA+4,C' '                                                   
         BZ    LR70                                                             
         MVC   LDRSTAT+5(1),RDIRSTA+4                                           
*                                  PERIOD                                       
LR70     ZAP   WORK+8(4),=P'0'                                                  
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
         GOTO1 DATCON,DMCB,(8,WORK),(5,LDRPERI+9)                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              NOTES                                        
         USING RDIRNTED,R6                                                      
         MVI   ELCODE,RDIRNCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RDIRNELN,RDIRNTOV                                                
         BNH   LR90                                                             
*                                                                               
         ZIC   R1,RDIRNELN         NOTE HAS VARIABLE LENGTH                     
         LA    RF,RDIRNTOV         OVERHEAD LENGTH                              
         SR    R1,RF               NOTE LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         CH    R1,=H'39'           MAX LEN IS 40                                
         BNH   LR80                                                             
         LA    R1,39                                                            
*                                                                               
LR80     DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDRCODE(0),RDIRNOTE                                              
         DROP  R6                                                               
*                                                                               
LR90     DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE STANDARD COMMENT                                                     
***********************************************************************         
VALICMT  NTR1                                                                   
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,AGENCY                                                  
         MVC   RCMTKOFF,=X'FFFF'                                                
         OC    10(L'RCMTKCDE,R2),SPACES BLANK PAD                               
         MVC   RCMTKCDE,10(R2)                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   INVLFLD             RECORD NOT FOUND                             
*                                                                               
         XC    18(L'DRNTXT-10,R2),18(R2) CLEAR REST                             
         MVI   5(R2),10            SET LENGTH                                   
*                                                                               
VALCMTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY STANDARD COMMENT                                                      
* R2 POINTS TO FIELD WITH COMMENT                                               
* USES IO3                                                                      
***********************************************************************         
DISCMT   NTR1                                                                   
         MVC   SVAIO,AIO                                                        
         MVC   SVELCODE,ELCODE     ELCODE GETS CLOBBERED                        
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,AGENCY                                                  
         MVC   RCMTKOFF,=X'FFFF'                                                
         OC    10(L'RCMTKCDE,R2),SPACES BLANK PAD                               
         MVC   RCMTKCDE,10(R2)                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    DISCMT10            RECORD NOT FOUND                             
*                                                                               
         MVC   20(28,R2),=C'* COMMENT RECORD NOT FOUND *'                       
         B     DISCMTX                                                          
*                                                                               
DISCMT10 DS    0H                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCMTELM2,R6                                                      
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    DISCMT20                                                         
         MVC   20(28,R2),=C'* COMMENT RECORD NOT FOUND *'                       
         B     DISCMTX                                                          
*                                                                               
DISCMT20 DS    0H                                                               
         CLI   RCMT2LEN,3          DISPLAY FIRST NON-BLANK LINE                 
         BH    DISCMT25                                                         
         BAS   RE,NEXTEL                                                        
         BE    DISCMT20                                                         
         B     DISCMTX                                                          
*                                                                               
DISCMT25 DS    0H                                                               
         ZIC   R1,RCMT2LEN         TOTAL LEN                                    
         SH    R1,=H'2'            SUB 2 FOR CODE AND LEN                       
         CH    R1,=H'48'                                                        
         BNH   DISCMT30                                                         
         LA    R1,48               MAX 48 CHARACTERS TO DISPLAY                 
*                                                                               
DISCMT30 DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   20(0,R2),RCMT2TXT                                                
*                                                                               
DISCMTX  DS    0H                                                               
         MVC   AIO,SVAIO                                                        
         MVC   ELCODE,SVELCODE                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT - R2 POINTS TO FIELD HEADER                                             
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
         B     EXIT                                                             
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLFILT MVC   RERROR,=AL2(INVFIL)                                              
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(INVGRP)                                              
         B     ERREND                                                           
*                                                                               
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
*                                                                               
RECNOTF  MVC   RERROR,=AL2(ADDNOTES)                                            
         B     ERREND                                                           
*                                                                               
INVLMAST MVC   RERROR,=AL2(INVMAST)                                             
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(INVPER)                                              
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
ADDLINE  EQU   5                                                                
DELLINE  EQU   6                                                                
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
XSVKEY   DS    CL(L'KEY)           SAVE KEY FOR POST ADD/UPDATE USE             
SAVEKEY  DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SVAIO    DS    A                   SAVE IO ADDRESS                              
SVELCODE DS    X                   SAVE ELCODE VALUE                            
SEQNUM   DS    X                                                                
STATION  DS    CL5                                                              
DRSTAT   DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC7D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC9D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENDIR                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE REGENGRP                                                       
         PRINT ON                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LDRSTAT  DS    CL7                                                              
         DS    CL2                                                              
LDRPERI  DS    CL17                                                             
         DS    CL2                                                              
LDRCODE  DS    CL40                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011RESFM26   05/01/02'                                      
         END                                                                    
