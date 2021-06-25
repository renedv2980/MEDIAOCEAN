*          DATA SET RESFM29    AT LEVEL 004 AS OF 10/21/93                      
*PHASE T81829A                                                                  
         TITLE 'T81829 - RESFM29 - GOALN RECORD'                                
***********************************************************************         
*                                                                     *         
*  RESFM29 (T81829) --- MAINTENANCE/LIST OF GOALN CARDS               *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 26MAY93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
*HERE******************************************************************         
T81829   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81829*,R7,RR=R3                                              
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
         XC    GOLSTAT,GOLSTAT     STATUS BITS                                  
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
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK100                                                            
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,GNLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   GNLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK50                                                             
*                                                                               
         CLI   GNLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
*                                                                               
         OC    GNLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R5,KEY                                                           
         USING RGRPKEY,R5                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,GNLGRUP                                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RGRPKEY),KEYSAVE                                           
         BNE   INVLGRP                                                          
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     VK60                                                             
*                                                                               
VK50     DS    0H                  FILTER ON STATION ?                          
         CLI   GNLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         LA    R2,GNLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
*                                                                               
VK60     DS    0H                  FILTER ON PERIOD?                            
         CLI   GNLPERIH+5,0                                                     
         BE    VKX                                                              
         LA    R2,GNLPERIH                                                      
         BAS   RE,VALIPERI                                                      
         B     VKX                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION CALL LETTERS                                                 
***********************************************************************         
VK100    DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,GLNSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   RGOLSTA,WORK        VALISTA PUTS OUTPUT IN WORK                  
         MVC   EGOLSTAT,GLNSTAT    SAVE IN CASE WE PF TO NOTE SCREEN            
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
         MVC   GLNLOCA(19),=C'*STA REC NOT FOUND*'                              
*                                                                               
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BNE   VK110                                                            
*                                                                               
         MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   VK110                                                            
         MVC   GLNLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
*                                                                               
VK110    OI    GLNLOCAH+6,X'80'    XMIT                                         
*                                                                               
         MVC   KEY,SVKEY                                                        
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
***********************************************************************         
VK120    DS    0H                                                               
         MVC   SVKEY,KEY           VALIPER USES KEY                             
         LA    R2,GLNPERIH         VALIDATE PERIOD                              
         BAS   RE,VALIPERI                                                      
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   RGOLENDT,ENDDT                                                   
         MVC   RGOLSTDT,STARTDT                                                 
         MVC   EGOLPERI,GLNPERI    SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
VKX      DS    0H                                                               
         OI    GLNSTATH+4,X'20'    STATION HAS BEEN VALIDATED                   
         OI    GLNPERIH+4,X'20'    PERIOD HAS BEEN VALIDATED                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,GLNSGOLH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD             MUST ENTER A SHARE GOAL                      
*                                                                               
         LA    R2,GLNCTIDH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD             MUST ENTER A CONTROL ID                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,RGOLDCDQ     REMOVE OLD DESCRIPTIVE ELEMENT               
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RGOLDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RGOLDCDE,RGOLDCDQ   ELEMENT CODE                                 
         MVI   RGOLDELN,RGOLDOV    OVERHEAD LENGTH                              
         MVC   RGOLDCID,GLNCTID    CONTROL ID                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RGOLDLUP)  LAST UPDATE DATE                 
         LA    R2,GLNSGOLH                                                      
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         STCM  R0,3,RGOLDSHR       SHARE GOAL                                   
*                                                                               
         LA    R2,GLNDESCH         DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RGOLDESC(0),GLNDESC                                              
*                                                                               
         ZIC   RF,RGOLDELN         ADD VARIABLE LENGTH FROM                     
         LA    RF,1(R1,RF)         DESCRIPTION FIELD                            
         STC   RF,RGOLDELN                                                      
         DROP  R6                                                               
*                                                                               
VR20     DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR30     DS    0H                                                               
         CLI   PFKEY,ADDLINE       ADD/DELETE LINE?                             
         BE    VR40                                                             
         CLI   PFKEY,DELLINE                                                    
         BNE   VR200                                                            
*                                                                               
VR40     DS    0H                                                               
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
         NI    GOLSTAT,X'FF'-CURSSTT-CURSKIN-CURSCMT                            
         OI    GOLSTAT,CURSSTT     CURSOR IS IN STT SECTION                     
         LA    R2,GLNHSTTH         1ST FLD WHICH COULD CONTAIN CURSOR           
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
         LA    RF,GLNHKINH                                                      
         CR    R2,RF               KEY INVENTORY TITLE FIELD?                   
         BNE   VR60                YES                                          
         NI    GOLSTAT,X'FF'-CURSSTT                                            
         OI    GOLSTAT,CURSKIN     CURSOR IS IN KEY & INV SECTION               
         B     VR50                                                             
*                                                                               
VR60     DS    0H                                                               
         LA    RF,GLNHCMTH                                                      
         CR    R2,RF               COMMENT FIELD?                               
         BNE   VR70                YES                                          
         NI    GOLSTAT,X'FF'-CURSKIN                                            
         OI    GOLSTAT,CURSCMT     CURSOR IS IN CMT SECTION                     
         B     VR50                                                             
*                                                                               
VR70     DS    0H                                                               
         LA    RF,GLNLCMTH                                                      
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
         LA    R0,GLNHSTTH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
         LA    R0,GLNHKINH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
         LA    R0,GLNHCMTH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
*                                                                               
VR110    DS    0H                                                               
         TM    GOLSTAT,CURSSTT                                                  
         BZ    VR115                                                            
         LA    RF,GLNLSTTH                                                      
         B     VR130                                                            
*                                                                               
VR115    TM    GOLSTAT,CURSKIN                                                  
         BZ    VR120                                                            
         LA    RF,GLNLKINH                                                      
         B     VR130                                                            
*                                                                               
VR120    TM    GOLSTAT,CURSCMT                                                  
         BNZ   *+6                                                              
         DC    H'0'                CAN'T FIND CURSOR, DUMP ME!                  
         LA    RF,GLNLCMTH                                                      
*                                                                               
VR130    DS    0H                                                               
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR140               YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'GLNSTT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR110                                                            
*                                                                               
VR140    XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'GLNSTT                                                      
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
         LA    RF,GLNLSTTH         ARE THEY TRYING TO INSERT AFTER END          
         CR    R2,RF               OF STR & TACT?                               
         BE    VR200               YES                                          
         LA    RF,GLNLKINH                                                      
         CR    R2,RF               OF KEY INV?                                  
         BE    VR200               YES                                          
         LA    RF,GLNLCMTH                                                      
         CR    R2,RF               OF COMMENT?                                  
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    RF,R2               SAVE A(INSERTION)                            
         TM    GOLSTAT,CURSSTT                                                  
         BZ    VR153                                                            
         LA    R3,GLNLSTTH                                                      
         B     VR158                                                            
*                                                                               
VR153    TM    GOLSTAT,CURSKIN                                                  
         BZ    VR155                                                            
         LA    R3,GLNLKINH                                                      
         B     VR158                                                            
*                                                                               
VR155    TM    GOLSTAT,CURSCMT                                                  
         BNZ   *+6                                                              
         DC    H'0'                CAN'T FIND CURSOR, DUMP ME!                  
         LA    R3,GLNLCMTH         LAST LINE OF TEXT                            
*                                                                               
VR158    DS    0H                                                               
         LR    R2,R3                                                            
*                                                                               
VR160    ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR170               YES                                          
         LA    R1,L'GLNSTT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR160                                                            
*                                                                               
VR170    XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'GLNSTT                                                      
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
         MVI   ELCODE,RGOLSCDQ     REMOVE OLD STRATEGY & TACTICS                
         GOTO1 REMELEM                                                          
         MVI   ELCODE,RGOLKCDQ     REMOVE OLD INVENTORY                         
         GOTO1 REMELEM                                                          
         MVI   ELCODE,RGOLCCDQ     REMOVE OLD COMMENT                           
         GOTO1 REMELEM                                                          
*                                                                               
VR210    DS    0H                                                               
         LA    R2,GLNSTTH          FIRST TEXT FIELD                             
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
         BAS   RE,VALICMT          VALIDATE FILE COMMENT RECORD                 
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
         BE    VRX                                                              
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VR215                                                            
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   GLNSTAT(4),RGOLSTA                                               
         MVC   GLNSTAT+4(2),=C'-T'                                              
         CLI   RGOLSTA+4,C' '                                                   
         BZ    DK05                                                             
         MVC   GLNSTAT+5(1),RGOLSTA+4                                           
DK05     OI    GLNSTATH+6,X'80'    XMIT                                         
         MVC   EGOLSTAT,GLNSTAT    SAVE IN CASE WE PF TO NOTE SCREEN            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,GLNPERI)                                 
*                                                                               
         MVI   GLNPERI+8,C'-'                                                   
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,GLNPERI+9)                               
         OI    GLNPERIH+6,X'80'    XMIT                                         
         MVC   EGOLPERI,GLNPERI    SAVE IN CASE WE PF TO NOTE SCREEN            
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
         MVC   GLNLOCA(19),=C'*STA REC NOT FOUND*'                              
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
         MVC   GLNLOCA,RSTAMKT                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
DK20     OI    GLNLOCAH+6,X'80'    XMIT                                         
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
         TWAXC GLNSGOLH            CLEAR SCREEN                                 
*                                                                               
         L     R6,AIO                                                           
         USING RGOLDESD,R6                                                      
         MVI   ELCODE,RGOLDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
*                                                                               
*                                  LAST UPDATE DATE                             
         GOTO1 DATCON,DMCB,(3,RGOLDLUP),(5,GLNLDAT)                             
         OI    GLNLDATH+6,X'80'    XMIT                                         
*                                                                               
*                                  SHARE GOAL                                   
         EDIT  RGOLDSHR,(3,GLNSGOL),ALIGN=LEFT                                  
         OI    GLNSGOLH+6,X'80'    XMIT                                         
*                                                                               
*                                  CONTROL ID                                   
         MVC   GLNCTID,RGOLDCID                                                 
         OI    GLNCTIDH+6,X'80'    XMIT                                         
*                                                                               
         CLI   RGOLDELN,RGOLDOV    ANY DESCRIPTION?                             
         BNH   DR10                                                             
*                                                                               
         ZIC   R1,RGOLDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RGOLDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLNDESC(0),RGOLDESC                                              
         OI    GLNDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
DR10     DS    0H                                                               
         LA    R2,GLNSTTH          FIRST TEXT FIELD                             
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
         BAS   RE,DISCMT                                                        
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
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SAVEKEY         USE SELECTED KEY                             
         XC    SAVEKEY,SAVEKEY                                                  
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
         CLI   GNLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RGOLSTA,STATION     YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(15),KEYSAVE     MATCHING REC TYPE AND REP CODE               
         BNE   LRX                                                              
*                                                                               
         CLI   GNLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   KEY+15(5),KEYSAVE+15                                             
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
LR30     CLI   KEY+26,0            MUST BE MASTER RECORD                        
         BNE   LRSEQ                                                            
*                                                                               
         CLI   GNLGRUPH+5,0        FILTER ON GROUP?                             
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
         ZIC   R1,GNLGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),GNLGRUP                                              
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
         CLI   GNLPERIH+5,0        FILTER ON PERIOD?                            
         BE    LR70                                                             
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
LR70     DS    0H                                                               
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
         BE    LR75                                                             
         MVC   LGOLSTAT+5(1),RGOLSTA+4                                          
*                                  PERIOD                                       
LR75     DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
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
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,LGOLPERI+9)                              
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              DESCRIPTION                                  
         USING RGOLDESD,R6                                                      
         MVI   ELCODE,RGOLDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RGOLDELN,RGOLDOV                                                 
         BNH   LR90                                                             
*                                                                               
         ZIC   R1,RGOLDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RGOLDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         CH    R1,=H'19'           MAX LEN IS 20                                
         BNH   LR80                                                             
         LA    R1,19                                                            
*                                                                               
LR80     DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LGOLDESC(0),RGOLDESC                                             
*                                                                               
LR90     DS    0H                  SHARE GOAL AND LAST UPDATE DATE              
         EDIT  RGOLDSHR,(8,LGOLSHGL),ALIGN=LEFT                                 
         GOTO1 DATCON,DMCB,(3,RGOLDLUP),(5,LGOLLUPD)                            
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
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
         CLI   ACTNUM,ACTADD                                                    
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
         CLI   KEY+26,0            MUST BE MASTER REC                           
         BNE   VALPER30                                                         
         CLC   STARTDT,KEY+23      CAN'T OVERLAP                                
         BE    INVLDOVL                                                         
         CLC   ENDDT,KEY+20                                                     
         BE    INVLDOVL                                                         
*                                                                               
         CLC   ENDDT,KEY+20        IS NEW END DATE EARLIER THAN                 
         BH    VALPER20            EXISTING END DATE?                           
*                                                                               
         CLC   STARTDT,KEY+20      NO, NEW START DATE HAS TO BE                 
         BL    VALPER30            LATER THAN EXISTING END DATE                 
         B     INVLDOVL                                                         
*                                                                               
VALPER20 CLC   ENDDT,KEY+23        YES, NEW END DATE HAS TO BE                  
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
* VALIDATE STANDARD COMMENT                                                     
* R2 POINTS TO FIELD HEADER                                                     
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
         XC    18(L'GLNCMT-10,R2),18(R2) CLEAR REST                             
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
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO3                                                         
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
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
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
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
**********************************************************************          
* LOCAL STORAGE AREA                                                            
**********************************************************************          
* USE TO VALIDATE S&T, KI AND CMTS                                              
* CL1 ELEMENT CODE                                                              
* CL1 NUMBER OF LINES TO PROCESS                                                
PROCTAB  DC    AL1(RGOLSCDQ),AL1(6)                                             
         DC    AL1(RGOLKCDQ),AL1(3)                                             
         DC    AL1(RGOLCCDQ),AL1(3)                                             
         DC    X'FF'                                                            
*                                                                               
ADDLINE  EQU   5                                                                
DELLINE  EQU   6                                                                
RELO     DS    A                                                                
SVELCODE DS    X                                                                
SVAIO    DS    A                                                                
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
SAVEKEY  DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
AINSERT  DS    F                   A(INSERTION)                                 
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SEQNUM   DS    X                                                                
STATION  DS    CL5                 STATION CALL LETTERS                         
GOLSTAT  DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
PFKEYUSE EQU   X'04'               PFKEY WAS USED                               
CURSSTT  EQU   X'08'               CURSOR IN STR & TAC SECTION                  
CURSKIN  EQU   X'10'               CURSOR IN KEY INV SECTION                    
CURSCMT  EQU   X'20'               CURSOR IN COMMENT SECTION                    
KEYCHGED DS    C                   KEY HAS CHANGED Y/N                          
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCDD          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCED          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENGOL                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENCMT                                                       
         PRINT ON                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LGOLSTAT DS    CL7                                                              
         DS    CL2                                                              
LGOLDESC DS    CL20                                                             
         DS    CL2                                                              
LGOLPERI DS    CL17                                                             
         DS    CL2                                                              
LGOLSHGL DS    CL8                                                              
         DS    CL2                                                              
LGOLLUPD DS    CL8                                                              
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RESFM29   10/21/93'                                      
         END                                                                    
