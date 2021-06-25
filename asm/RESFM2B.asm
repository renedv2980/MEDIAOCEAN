*          DATA SET RESFM2B    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T8182BA                                                                  
         TITLE 'T8182B - RESFM2B - GOAL'                                        
***********************************************************************         
*                                                                     *         
*  RESFM2B (T8182B) --- INPUT OF GOAL CARDS                           *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 26MAY93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 01NOV93 (SKU) ADD PAGE 2 RECORD                                     *         
*                                                                     *         
*HERE******************************************************************         
T8182B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8182B*,R7,RR=R3                                              
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
                                                                                
         OC    PAGENUM,PAGENUM     INIT PAGE NUMBER                             
         BNZ   *+8                                                              
         MVI   PAGENUM,1                                                        
                                                                                
         TM    EGOLSTUS,EGOLLTPF   IF PFKEY 5/6 WAS PRESSED IN                  
         BZ    MAIN10              LIST CHANGE, GOTO VALREC SINCE               
         NI    EGOLSTUS,X'FF'-EGOLLTPF  CONTROLLER RESTORES RECORD              
         B     VR                                                               
                                                                                
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
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RGOLKEY,R4                                                       
         MVI   RGOLTYP,RGOLTYPQ                                                 
         MVC   RGOLREP,AGENCY                                                   
         MVC   RGOLMAST,PAGENUM    ACCOUNT/OFFICE RECORD                        
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK30                                                             
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,GLLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   GLLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK10                                                             
*                                                                               
         CLI   GLLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
*                                                                               
         OC    GLLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R5,KEY                                                           
         USING RGRPKEY,R5                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,GLLGRUP                                                 
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
         CLI   GLLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         LA    R2,GLLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
*                                                                               
VK20     DS    0H                  FILTER ON PERIOD?                            
         CLI   GLLPERIH+5,0                                                     
         BE    VKX                                                              
         LA    R2,GLLPERIH                                                      
         BAS   RE,VALIPERI                                                      
         B     VKX                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK30     DS    0H                                                               
         TM    GOLSTATH+4,X'20'                                                 
         BZ    VK33                                                             
         TM    GOLPERIH+4,X'20'                                                 
         BO    VK34                                                             
                                                                                
VK33     DS    0H                  KEY FIELDS WERE CHANGED, INIT PAGE #         
         MVI   PAGENUM,1                                                        
                                                                                
VK34     DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,GOLSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   RGOLSTA,WORK        SAVE CALL LETTERS                            
         MVC   EGOLSTAT,GOLSTAT    SAVE IN CASE WE PF TO MASTER SCRN            
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVKEY+15                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   GOLLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   VK35                                                             
*                                                                               
         MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   VK35                                                             
         MVC   GOLLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
VK35     DS    0H                                                               
         OI    GOLSTATH+4,X'20'                                                 
         OI    GOLLOCAH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,GOLPERIH         VALIDATE PERIOD                              
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
         GOTO1 DATCON,DMCB,WORK,(1,STARTDT)      START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,22(R5),WORK+6                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
         GOTO1 DATCON,DMCB,WORK+6,(1,ENDDT)      END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   RGOLENDT,WORK                                                    
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   RGOLSTDT,WORK                                                    
         MVC   EGOLPERI,GOLPERI    SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
         OI    GOLPERIH+4,X'20'                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VK40                                                             
         CLI   ACTNUM,ACTREST                                                   
         BE    VKX                                                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE     RECORD NOT FOUND                      
         BE    VKX                                                              
         LA    R2,CONACTH                                                       
         B     RECNOTF                                                          
*                                                                               
* FOR ACTION ADD, CHECK IF MASTER RECORD EXISTS                                 
*                                                                               
VK40     DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2                                                         
         XC    RGOLMAST,RGOLMAST                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE     MASTER RECORD MUST BE THERE           
         BNE   INVLMAST                                                         
*                                                                               
         CLI   GOLCTIDH+5,0                                                     
         BNE   VK50                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RGOLDESD,R6                                                      
         MVI   ELCODE,RGOLDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
*                                                                               
         GOTO1 DATCON,DMCB,(3,RGOLDLUP),(5,GOLLDAT)                             
         OI    GOLLDATH+6,X'80'    XMIT                                         
*                                                                               
         EDIT  RGOLDSHR,(8,GOLSGOL),ALIGN=LEFT                                  
         MVI   GOLSGOLH+5,L'GOLSGOL                                             
         OI    GOLSGOLH+6,X'80'    XMIT                                         
*                                                                               
         MVC   GOLCTID,RGOLDCID                                                 
         MVI   GOLCTIDH+5,L'GOLCTID                                             
         OI    GOLCTIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   RGOLDELN,RGOLDOV    ANY DESCRIPTION?                             
         BNH   VK50                                                             
*                                                                               
         ZIC   R1,RGOLDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RGOLDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
*                                                                               
         STC   R1,GOLDESCH+5       INSERT LENGTH                                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOLDESC(0),RGOLDESC                                              
         OI    GOLDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
VK50     MVC   KEY,SVKEY           RESTORE KEY/AIO                              
         MVC   AIO,AIO1                                                         
*                                                                               
VKX      DS    0H                                                               
         MVC   GOALKEY,KEY                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE THE MASTER RECORD                                                      
***********************************************************************         
XRP      DS    0H                                                               
         MVC   KEY,GOALKEY                                                      
         LA    R6,KEY                                                           
         USING RGOLREC,R6                                                       
         XC    RGOLMAST,RGOLMAST   UPDATE DESCRIPTIVE ELEMENT IN MASTER         
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BNE   INVLMAST            MASTER RECORD NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              UPDATE LAST CHANGE DATE                      
         USING RGOLREC,R6                                                       
         GOTO1 DATCON,DMCB,(5,0),(3,RGOLDLUP)  LAST UPDATE DATE                 
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC              UPDATE MASTER                                
         MVC   AIO,AIO1            RESTORE IO AREA AND KEY                      
*                                                                               
XRPX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR20                                                             
         CLI   ACTNUM,ACTREST                                                   
         BE    VR20                                                             
         TM    GOALSTAT,NEWPAGE                                                 
         BO    VR20                                                             
                                                                                
         MVC   KEY,GOALKEY         READ PAGE INTO IOAREA                        
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVC   RGOLMAST,PAGENUM                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BE    *+6                 CAN'T FIND RECORD                            
         DC    H'0'                MUST BE THERE                                
                                                                                
         GOTO1 GETREC                                                           
                                                                                
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
         LA    R2,GOLHACCH         1ST FIELD WHICH COULD CONTAIN CURSOR         
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
         LA    RF,GOLLACCH                                                      
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
         LA    R0,GOLHACCH                                                      
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
VR80     LA    RF,GOLLACCH         A(LAST TEXT FIELD)                           
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
         LA    RF,GOLLACCH                                                      
         CR    R2,RF                                                            
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         ST    R2,AINSERT          SAVE A(INSERTION)                            
         LA    R3,GOLLACCH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR130    DS    0H                                                               
         LA    RF,GOLACCH          USER INSERT LINE AT PROTECTED HEADER         
         CR    R2,RF               CHECK SO WE WON'T GO PAST FIRST LINE         
         BE    VR150                                                            
*                                                                               
         LA    R0,GOLACC2H         COMPUTE LENGTH OF ONE LINE                   
         LA    R1,GOLACCH                                                       
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
         EJECT                                                                  
**********************************************************************          
* ADD/UPDATE ACCOUNT/OFFICE ELEMENT                                             
**********************************************************************          
VR200    DS    0H                                                               
         L     R6,AIO                                                           
         USING RGOLREC,R6                                                       
                                                                                
         TM    GOALSTAT,NEWPAGE                                                 
         BZ    VR205                                                            
         MVC   RGOLMAST,PAGENUM                                                 
         DROP  R6                                                               
                                                                                
VR205    DS    0H                                                               
         MVI   ELCODE,RGOLGCDQ                                                  
         GOTO1 REMELEM                                                          
                                                                                
VR210    DS    0H                                                               
         LA    R2,GOLACCH                                                       
         MVI   SEQNUM,1                                                         
         LA    R4,12               NUMBER OF LINES TO PROCESS                   
         NI    GOALSTAT,X'FF'-NOBLNKLN FLAG FOR INPUT                           
                                                                                
VR220    DS    0H                                                               
         LR    RF,R2               1ST FIELD OF LINE TO CHECK FOR INPUT         
         LA    RE,5                THERE WILL BE 5 TOTAL TO CHECK               
                                                                                
VR230    DS    0H                  ADD ELEMENT IF LINE HAS ENTRIES              
         CLI   5(RF),0                                                          
         BNE   VR240                                                            
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCT   RE,VR230                                                         
                                                                                
         LR    R2,RF               NO INPUT ON THIS LINE                        
         B     VR260                                                            
                                                                                
VR240    DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RGOLGOLD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RGOLGCDE,RGOLGCDQ                                                
         MVI   RGOLGELN,RGOLGELQ                                                
         MVC   RGOLGSQ,SEQNUM                                                   
                                                                                
         CLI   5(R2),0             ACCOUNT                                      
         BE    MISSFLD                                                          
         MVC   RGOLGACC,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             OFFICE                                       
         BE    MISSFLD                                                          
         BAS   RE,VALOFF           VALIDATE OFFICE                              
         BNE   INVLFLD                                                          
         MVC   RGOLGOFF,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             PRIOR SHARE                                  
         BE    *+10                                                             
         MVC   RGOLGPSH,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             PRIOR DOLLARS                                
         BE    *+10                                                             
         MVC   RGOLGPDO,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             SHARE GOAL                                   
         BE    MISSFLD                                                          
         MVC   RGOLGSHR,8(R2)                                                   
         DROP  R6                                                               
                                                                                
         ZIC   R0,0(R2)            BUMP TO NEXT                                 
         AR    R2,R0                                                            
                                                                                
         OI    GOALSTAT,NOBLNKLN   SOME TEXT WAS FOUND                          
                                                                                
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR260    DS    0H                                                               
         ZIC   RF,SEQNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         BCT   R4,VR220                                                         
                                                                                
         LA    R2,GOLACCH          CHECK IF ANY INPUT AT ALL                    
         TM    GOALSTAT,NOBLNKLN                                                
         BZ    MISSFLD             MUST HAVE AT LEAST ONE LINE OF INPUT         
                                                                                
VR400    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR403                                                            
         MVC   CONACT(8),=C'CHANGE  '                                           
         OI    CONACTH+6,X'80'     XMIT                                         
         B     VR405                                                            
         DC    H'0'                                                             
                                                                                
VR403    DS    0H                  IF NEW PAGE                                  
         TM    GOALSTAT,NEWPAGE                                                 
         BZ    VR410                                                            
                                                                                
VR405    DS    0H                                                               
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
         CLI   DMCB+8,0                                                         
         BE    VRX                                                              
         DC    H'0'                                                             
                                                                                
VR410    DS    0H                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
                                                                                
         CLC   KEY(L'RGOLKEY),SELCKEY                                           
         BE    DK03                                                             
         CLI   ACTNUM,ACTSEL       INCASE WE ARE DISPLAYING A LIST              
         BNE   DK03                OF RECORDS                                   
         CLI   TWALACT,ACTSEL                                                   
         BNE   DK03                                                             
         MVC   PAGENUM,RGOLMAST    THIS GETS THE CORRECT PAGE                   
                                                                                
DK03     DS    0H                                                               
         MVC   SELCKEY,KEY                                                      
         MVC   GOALKEY,KEY                                                      
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   GOLSTAT(4),RGOLSTA                                               
         MVC   GOLSTAT+4(2),=C'-T'                                              
         CLI   RGOLSTA+4,C' '                                                   
         BZ    DK05                                                             
         MVC   GOLSTAT+5(1),RGOLSTA+4                                           
DK05     OI    GOLSTATH+6,X'80'    XMIT                                         
         MVC   EGOLSTAT,GOLSTAT    SAVE IN CASE WE PF TO MASTER SCRN            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,GOLPERI)                                 
*                                                                               
         MVI   GOLPERI+8,C'-'                                                   
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,GOLPERI+9)                               
         OI    GOLPERIH+6,X'80'    XMIT                                         
         MVC   EGOLPERI,GOLPERI    SAVE IN CASE WE PF TO NOTE SCREEN            
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVKEY+15                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   GOLLOCA(19),=C'*STA REC NOT FOUND*'                              
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
         MVC   GOLLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
DK20     OI    GOLLOCAH+6,X'80'    XMIT                                         
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
         NI    GOALSTAT,X'FF'-NEWPAGE                                           
         TWAXC GOLACCH             CLEAR SCREEN                                 
                                                                                
         MVC   AIO,AIO2                                                         
         MVC   KEY,GOALKEY                                                      
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         XC    RGOLMAST,RGOLMAST                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BNE   INVLMAST            CAN'T FIND MASTER RECORD                     
         DROP  R6                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RGOLDESD,R6                                                      
         MVI   ELCODE,RGOLDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
                                                                                
         GOTO1 DATCON,DMCB,(3,RGOLDLUP),(5,GOLLDAT)                             
         OI    GOLLDATH+6,X'80'    XMIT                                         
                                                                                
         EDIT  RGOLDSHR,(3,GOLSGOL),ALIGN=LEFT                                  
         OI    GOLSGOLH+6,X'80'    XMIT                                         
                                                                                
         MVC   GOLCTID,RGOLDCID                                                 
         OI    GOLCTIDH+6,X'80'    XMIT                                         
                                                                                
         CLI   RGOLDELN,RGOLDOV    ANY DESCRIPTION?                             
         BNH   DR10                                                             
                                                                                
         ZIC   R1,RGOLDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RGOLDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOLDESC(0),RGOLDESC                                              
         OI    GOLDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
                                                                                
DR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVI   RGOLMAST,1                                                       
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
DR20     DS    0H                  FIND TOTAL PAGES IN THIS RECORD              
         CLC   KEY(L'RGOLKEY-1),KEYSAVE                                         
         BNE   DR30                                                             
         GOTO1 SEQ                                                              
         B     DR20                                                             
                                                                                
DR30     DS    0H                                                               
         LA    R6,KEYSAVE                                                       
         USING RGOLKEY,R6                                                       
         MVC   TOTPAGES,RGOLMAST                                                
         DROP  R6                                                               
                                                                                
         MVI   GOLPAGE+3,C'/'                                                   
         LA    R2,GOLPAGEH                                                      
         EDIT  TOTPAGES,(3,12(R2)),FILL=0                                       
         OI    GOLPAGEH+6,X'80'    XMIT                                         
                                                                                
         MVC   KEY,GOALKEY                                                      
         MVC   AIO,AIO1                                                         
         CLI   ACTNUM,ACTADD       IF RECORD ADD, IGNORE PFKEYS                 
         BE    DR90                                                             
                                                                                
* PF 7 WAS HIT = PAGE UP                                                        
                                                                                
         CLI   PFKEY,PAGEUP                                                     
         BNE   DR40                                                             
         CLI   PAGENUM,1           ALREADY AT TOP OF PAGE                       
         BE    DR90                                                             
         ZIC   RF,PAGENUM                                                       
         BCTR  RF,0                                                             
         STC   RF,PAGENUM                                                       
         B     DR50                                                             
                                                                                
* PF 8 WAS HIT = PAGE DOWN                                                      
                                                                                
DR40     DS    0H                                                               
         CLI   PFKEY,PAGEDOWN                                                   
         BNE   DR90                                                             
                                                                                
         CLI   PAGENUM,255         MAXIMUM 255 PAGES                            
         BE    DR50                                                             
                                                                                
         ZIC   RF,PAGENUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,PAGENUM                                                       
                                                                                
         CLC   PAGENUM,TOTPAGES    ARE WE AT THE LAST PAGE?                     
         BNH   DR50                IF YES, CHECK IF ACTION DISPLAY              
         CLI   ACTNUM,ACTDIS       IF DISPLAY,                                  
         BE    DR45                                                             
         CLI   THISLSEL,C'S'       OR SELECT DISPLAY IN LIST                    
         BE    DR45                STAY ON SAME PAGE                            
                                                                                
         ZIC   RE,PAGENUM          FOR CHANGE, CAN ONLY DISPLAY NEXT            
         ZIC   RF,TOTPAGES         NEW PAGE                                     
         SR    RE,RF                                                            
         CH    RE,=H'2'                                                         
         BL    DR43                                                             
         ZIC   RF,PAGENUM          STAY ON SAME PAGE                            
         BCTR  RF,0                                                             
         STC   RF,PAGENUM                                                       
         B     DRX                                                              
                                                                                
DR43     DS    0H                                                               
         EDIT  PAGENUM,(3,GOLPAGE),FILL=0                                       
         OI    GOALSTAT,NEWPAGE                                                 
         B     DRX                                                              
                                                                                
DR45     DS    0H                                                               
         ZIC   RF,PAGENUM          STAY ON SAME PAGE                            
         BCTR  RF,0                                                             
         STC   RF,PAGENUM                                                       
                                                                                
DR50     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVC   RGOLMAST,PAGENUM                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
DR90     DS    0H                                                               
         EDIT  PAGENUM,(3,GOLPAGE),FILL=0                                       
                                                                                
         L     R6,AIO                                                           
         USING RGOLGOLD,R6                                                      
         MVI   ELCODE,RGOLGCDQ     GOAL ACCOUNT/OFFICE                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE AT LEAST ONE ACCOUNT/OFF           
                                                                                
DR100    DS    0H                                                               
         MVI   SEQNUM,1                                                         
         LA    R2,GOLACCH          FIRST LINE OF OUTPUT TEXT                    
                                                                                
DR110    DS    0H                                                               
         CLC   SEQNUM,RGOLGSQ      TEXT BELONG ON THIS LINE?                    
         BE    DR130                                                            
                                                                                
         LA    R5,5                                                             
DR120    ZIC   R0,0(R2)            NO, BUMP TO NEXT LINE                        
         AR    R2,R0                                                            
         BCT   R5,DR120                                                         
         B     DR140                                                            
                                                                                
DR130    DS    0H                  YES, DISPLAY THIS LINE                       
         MVC   8(L'RGOLGACC,R2),RGOLGACC                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(L'RGOLGOFF,R2),RGOLGOFF                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(L'RGOLGPSH,R2),RGOLGPSH                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(L'RGOLGPDO,R2),RGOLGPDO                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(L'RGOLGSHR,R2),RGOLGSHR                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
                                                                                
DR140    DS    0H                                                               
         ZIC   RF,SEQNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         B     DR110                                                            
                                                                                
DRX      DS    0H                                                               
         MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE THE RECORD                                                             
***********************************************************************         
DELR     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVI   RGOLMAST,1          PAGE 1 AND DOWN                              
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BNE   DELX                                                             
                                                                                
DELR10   DS    0H                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RGOLREC,R6                                                       
         OI    RGOLCNTL,X'80'      MARK FOR DELETION                            
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    KEY+27,X'80'        MARK FOR DELETION                            
                                                                                
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 SEQ                                                              
         CLC   KEY(L'RGOLKEY-1),KEYSAVE                                         
         BE    DELR10                                                           
                                                                                
DELX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE THE RECORD                                                            
***********************************************************************         
RESR     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVI   RGOLMAST,1          PAGE 1 AND DOWN                              
         DROP  R6                                                               
                                                                                
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BNE   RESX                                                             
                                                                                
RESR10   DS    0H                                                               
         OI    DMINBTS,X'08'       READ FOR DELETE                              
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RGOLREC,R6                                                       
         NI    RGOLCNTL,X'FF'-X'80'  RESTORE                                    
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    KEY+27,X'FF'-X'80'  RESTORE                                      
                                                                                
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    DMINBTS,X'08'       READ FOR DELETE                              
         GOTO1 SEQ                                                              
         CLC   KEY(L'RGOLKEY-1),KEYSAVE                                         
         BE    RESR10                                                           
                                                                                
RESX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         MVI   PAGENUM,1                                                        
                                                                                
         OC    SELCKEY,SELCKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SELCKEY         USE SELECTED KEY                             
         XC    SELCKEY,SELCKEY                                                  
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'RGOLKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RGOLTYP,RGOLTYPQ                                                 
         MVC   RGOLREP,AGENCY                                                   
*                                                                               
         CLI   GLLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RGOLSTA,STATION     YES, START WITH THIS STATION                 
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(15),KEYSAVE                                                  
         BNE   LRX                                                              
*                                                                               
         CLI   GLLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   KEY+15(5),KEYSAVE+15                                             
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
LR30     CLI   KEY+26,1            MUST BE NOTES RECORD                         
         BNE   LRSEQ                                                            
*                                                                               
         CLI   GLLGRUPH+5,0        FILTER ON GROUP?                             
         BE    LR60                                                             
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVKEY+15                                               
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
         ZIC   R1,GLLGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),GLLGRUP                                              
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
         CLI   GLLPERIH+5,0        FILTER ON PERIOD?                            
         BE    LR65                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         CLC   STARTDT,RGOLENDT    THESE DATES ARE IN 9'S COMP!                 
         BL    LRSEQ                                                            
         CLC   ENDDT,RGOLSTDT                                                   
         BH    LRSEQ                                                            
         DROP  R6                                                               
*                                                                               
LR65     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RGOLREC,R6                                                       
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LGOLSTAT(4),RGOLSTA STATION CALL LETTER                          
         MVC   LGOLSTAT+4(2),=C'-T'                                             
         CLI   RGOLSTA+4,C' '                                                   
         BZ    LR70                                                             
         MVC   LGOLSTAT+5(1),RGOLSTA+4                                          
*                                  PERIOD                                       
LR70     ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,LGOLPERI)                                
         MVI   LGOLPERI+8,C'-'                                                  
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,LGOLPERI+9)                              
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
* VALIDATE OFFICE                                                               
* INPUT - R2 POINTS TO FIELD HEADER                                             
* OUTPUT - CONDITION CODE SET                                                   
***********************************************************************         
VALOFF   NTR1                                                                   
         LA    R6,KEY                                                           
         USING ROFFKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,8(R2)                                                   
         OC    ROFFKOFF,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BE    OFFYES                                                           
*                                                                               
OFFNO    LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
OFFYES   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXIT                                                             
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
         GOTO1 DATCON,DMCB,WORK,(1,STARTDT)      START DATE                     
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
         GOTO1 DATCON,DMCB,WORK+6,(1,ENDDT)      END DATE                       
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
RECNOTF  MVC   RERROR,=AL2(387)                                                 
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
*                                                                               
* TABLE FOR LENGTH OF FIELDS IN ONE ACCOUNT/OFFICE LINE                         
*                                                                               
LENFLD   DS    0XL5                                                             
         DC    AL1(L'GOLACC),AL1(L'GOLOFF),AL1(L'GOLPSHR)                       
         DC    AL1(L'GOLPDOL),AL1(L'GOLSHRG)                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDBD          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDCD          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENGOL                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE REGENGRP                                                       
         PRINT ON                                                               
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
         ORG   SYSSPARE                                                         
ADDLINE  EQU   5                                                                
DELLINE  EQU   6                                                                
PAGEUP   EQU   7                                                                
PAGEDOWN EQU   8                                                                
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
SVKEY    DS    CL(L'KEY)           USED FOR TEMPORARY SAVE OF KEY               
SELCKEY  DS    CL(L'KEY)                                                        
GOALKEY  DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
SAVER6   DS    F                                                                
AINSERT  DS    F                   A(INSERTION)                                 
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SVAIO    DS    A                   SAVE IO ADDRESS                              
SVELCODE DS    X                   SAVE ELCODE VALUE                            
SEQNUM   DS    X                                                                
STATION  DS    CL5                                                              
*                                                                               
GOALSTAT DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
NEWPAGE  EQU   X'04'               USER HAS PAGED DOWN TO A NEW PAGE            
*                                                                               
PAGENUM  DS    X                   CURRENT PAGE NUMBER                          
TOTPAGES DS    X                   TOTAL PAGES                                  
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LGOLSTAT DS    CL7                                                              
         DS    CL2                                                              
LGOLPERI DS    CL17                                                             
*                                                                               
* OFFLINE LIST LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003RESFM2B   05/01/02'                                      
         END                                                                    
