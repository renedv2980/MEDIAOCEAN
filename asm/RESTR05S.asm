*          DATA SET RESTR05S   AT LEVEL 053 AS OF 10/10/96                      
*PHASE T80E05A                                                                  
         TITLE 'T80E05 - RESTR05 - ACCOUNTS'                                    
***********************************************************************         
*                                                                     *         
*  RESTR05 (T80E05) --- ACCOUNTS MAINTENANCE                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01FEB94 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
* 08MAR94 (SKU) ADD GROUP/SUBGROUP IN KEY                             *         
*                                                                     *         
* 17MAR95 (BU ) DISPLAY CREATION INFORMATION                          *         
*                                                                     *         
* 25APR95 (SKU) FIX BUG OF REDISPLAY AFTER ADDING NEW PAGE            *         
*                                                                     *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T80E05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80E05*,R7,RR=R3                                              
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
         GOTO1 INITIAL                                                          
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   DATAOVRD,C'N'       SET DATA OVERRIDES ENTERED TO NO             
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         MVI   IOOPT,C'Y'          DO MY OWN I/O 'S                             
                                                                                
         OC    PAGENUM,PAGENUM     INIT PAGE NUMBER                             
         BNZ   *+8                                                              
         MVI   PAGENUM,1                                                        
                                                                                
         TM    STRFLAGS,PFKEYHIT   IF PFKEY 2/3 WAS PRESSED IN                  
         BZ    MAIN10              LIST CHANGE, GOTO VALREC SINCE               
         NI    STRFLAGS,X'FF'-PFKEYHIT  CONTROLLER RESTORES RECORD              
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
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRACCQ   TYPE ACCOUNT                                 
         MVC   RSTRKREP,AGENCY                                                  
         MVC   RSTRKPG,PAGENUM                                                  
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK30                                                             
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,ACLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   ACLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK10                                                             
*                                                                               
         CLI   ACLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
                                                                                
         CLI   TWAACCS,C'$'        STA SIGN-ON MUST FILTER ON STA               
         BE    INVLSIGN                                                         
                                                                                
         OC    ACLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
         GOTO1 VALIGRP                                                          
         BNZ   INVLGRP                                                          
         MVC   GRPSGRP,ACLGRUP                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     VK20                                                             
*                                                                               
VK10     DS    0H                  FILTER ON STATION ?                          
         CLI   ACLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         MVC   SVKEY,KEY                                                        
         LA    R2,ACLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
         MVC   GRPSGRP,WORK+41                                                  
         MVC   KEY,SVKEY                                                        
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
*                                                                               
VK20     DS    0H                  FILTER ON PERIOD?                            
         CLI   ACLPERIH+5,0                                                     
         BE    VKX                                                              
         MVC   SVKEY,KEY                                                        
         LA    R2,ACLPERIH                                                      
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
         TM    CONACTH+4,X'20'     IF ACTION FIELD WAS CHANGED,                 
         BZ    VK33                 INIT PAGE #                                 
         TM    ACCSTATH+4,X'20'                                                 
         BZ    VK33                                                             
         TM    ACCPERIH+4,X'20'                                                 
         BO    VK34                                                             
                                                                                
VK33     DS    0H                                                               
         OI    CONACTH+4,X'20'                                                  
         MVI   PAGENUM,1           KEY FIELDS WERE CHANGED, INIT PAGE #         
         MVC   RSTRKPG,PAGENUM     UPDATE KEY                                   
         XC    ACCPAGE,ACCPAGE     CLEAR PAGE DISPLAY                           
         XC    ACCCREA,ACCCREA     CLEAR CREATE INFO DISPLAY                    
         OI    ACCPAGEH+6,X'80'    XMIT                                         
                                                                                
VK34     DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,ACCSTATH         VALIDATE STATION CALL LETTER FORMAT          
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
         MVC   ACCLOCA,WORK+10     MARKET NAME                                  
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
                                                                                
         OI    ACCSTATH+4,X'20'                                                 
         OI    ACCLOCAH+6,X'80'    XMIT                                         
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,ACCPERIH                                                      
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
                                                                                
VK50     CLC   KEY(9),KEYSAVE      OK IF DIFFERENT STATION                      
         BNE   VK100                                                            
         CLC   STARTDT,KEY+11      CAN'T OVERLAP                                
         BE    INVLDOVL                                                         
         CLC   ENDDT,KEY+9                                                      
         BE    INVLDOVL                                                         
                                                                                
         CLC   ENDDT,KEY+9         IS NEW END DATE EARLIER THAN                 
         BH    VK60                EXISTING END DATE?                           
                                                                                
         CLC   STARTDT,KEY+9       NO, NEW START DATE HAS TO BE                 
         BL    VK70                LATER THAN EXISTING END DATE                 
         B     INVLDOVL                                                         
                                                                                
VK60     CLC   ENDDT,KEY+11        YES, NEW END DATE HAS TO BE                  
         BNH   INVLDOVL            EARLIER THAN EXISTING START DATE             
                                                                                
VK70     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 SEQ                                                              
         B     VK50                                                             
                                                                                
VK100    DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVC   RSTRKSTD,WORK                                                    
         MVC   RSTRKEND,WORK+3                                                  
         MVC   STRPERI,ACCPERI     SAVE IN CASE WE PF TO OTHER SCRN             
                                                                                
         OI    ACCPERIH+4,X'20'                                                 
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK110                                                            
                                                                                
* DISPLAY SHARE GOAL, LAST UPD, DESC                                            
         MVC   SVKEY,KEY                                                        
         GOTO1 DISINFO,DMCB,ACCSGOLH,ACCLDATH,ACCDESCH                          
         MVC   KEY,SVKEY                                                        
                                                                                
VK110    DS    0H                                                               
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    VK120                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    VKX                                                              
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BE    VK120                                                            
         LA    R2,CONACTH          RECORD NOT FOUND                             
         B     RECNOTF                                                          
*                                                                               
* FOR ACTION ADD, CHECK IF SITUATION ANALYSIS RECORD EXISTS                     
*                                                                               
VK120    DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   RSTRKSUB,RSTRSITQ                                                
         XC    RSTRKPG,RSTRKPG                                                  
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE     SIT RECORD MUST BE THERE              
         BNE   NOSITREC                                                         
                                                                                
         MVC   KEY,SVKEY           RESTORE KEY/AIO                              
                                                                                
VKX      DS    0H                                                               
         MVC   ACCKEY,KEY                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE THE MASTER RECORD                                                      
***********************************************************************         
XRP      DS    0H                                                               
         MVC   KEY,ACCKEY                                                       
         GOTO1 UPDTLCHG            GO UPDATE LAST CHANGED DATE                  
         BNZ   NOSITREC            SIT ANA REC NOT FOUND                        
         MVC   KEY,ACCKEY                                                       
XRPX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
* USES AIO2 FOR UPDATING OF ACCOUNTS SUB RECORDS                                
***********************************************************************         
VR       DS    0H                                                               
         CLI   PFKEY,ADDLINE       ADD/DELETE LINE?                             
         BE    VR0030                                                           
         CLI   PFKEY,DELLINE                                                    
         BNE   VR0320                                                           
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE A LINE                                                             
***********************************************************************         
VR0030   DS    0H                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD, ADD FIRST CHANGE LATER           
         BE    VR0320                                                           
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
         LA    R2,ACCHACCH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR0040   SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR0320              NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR0080              YES                                          
*                                                                               
         LA    RE,L'LENFLD                                                      
VR0060   ZIC   RF,0(R2)            BUMP TO FIRST FIELD OF NEXT LINE             
         AR    R2,RF                                                            
         BCT   RE,VR0060                                                        
*                                                                               
         LA    RF,ACCLACCH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR0320              YES                                          
         B     VR0040                                                           
*                                                                               
VR0080   CLI   PFKEY,ADDLINE       JUMP TO ROUTINE                              
         BE    VR0200                                                           
         EJECT                                                                  
***********************************************************************         
* DELETE LINE                                                                   
***********************************************************************         
         LA    R0,ACCHACCH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR0320              YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
*                                                                               
         LA    RE,L'LENFLD                                                      
VR0100   ZIC   R0,0(R3)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         BCT   RE,VR0100                                                        
*                                                                               
VR0120   LA    RF,ACCLACCH         A(LAST TEXT FIELD)                           
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR0160              YES                                          
                                                                                
         LA    RE,LENFLD           MOVE ALL FIELDS FROM LOWER LINE              
         LA    RF,L'LENFLD         TO ONE BEFORE                                
VR0140   ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT IND, AND LEN                      
         ZIC   R0,0(R3)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR0140                                                        
         B     VR0120                                                           
*                                                                               
VR0160   LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR0180   XC    4(2,R2),4(R2)       CLEAR INPUT IND, AND LEN                     
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELDS                                 
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR0180                                                        
         B     VR0320                                                           
         EJECT                                                                  
***********************************************************************         
* ADD LINE                                                                      
***********************************************************************         
VR0200   DS    0H                  ARE THEY TRYING TO INSERT AFTER END?         
         LA    RF,ACCLACCH                                                      
         CR    R2,RF                                                            
         BE    VR0320              YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         ST    R2,AINSERT          SAVE A(INSERTION)                            
         LA    R3,ACCLACCH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR0220   DS    0H                                                               
         LA    RF,ACCACCH          USER INSERT LINE AT PROTECTED HEADER         
         CR    R2,RF               CHECK SO WE WON'T GO PAST FIRST LINE         
         BE    VR0280                                                           
*                                                                               
         LA    R0,ACCACC2H         COMPUTE LENGTH OF ONE LINE                   
         LA    R1,ACCACCH                                                       
         SR    R0,R1                                                            
         SR    R2,R0               R2 POINTS TO PREVIOUS LINE                   
*                                                                               
         L     RF,AINSERT                                                       
         CR    R2,RF               ARE WE AT THE CURSOR?                        
         BE    VR0280              YES                                          
*                                                                               
         ST    R2,SAVER2                                                        
*                                                                               
         LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR0240   ZIC   R1,0(RE)                                                         
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
VR0260   ZIC   R0,0(R3)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR0240                                                        
*                                                                               
         L     R2,SAVER2                                                        
         LR    R3,R2                                                            
         B     VR0220                                                           
*                                                                               
VR0280   LA    RE,LENFLD                                                        
         LA    RF,L'LENFLD                                                      
VR0300   XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR FIELDS                                 
         OI    6(R3),X'80'         XMIT THIS                                    
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR0300                                                        
         EJECT                                                                  
**********************************************************************          
* ADD/UPDATE ACCOUNT/OFFICE ELEMENT                                             
**********************************************************************          
VR0320   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR0340                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    VR0340                                                           
         TM    ACCFLAG,NEWPAGE                                                  
         BO    VR0340                                                           
                                                                                
         MVC   KEY,ACCKEY          READ PAGE INTO IOAREA                        
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVC   RSTRKPG,PAGENUM                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BE    *+6                 CAN'T FIND RECORD                            
         DC    H'0'                MUST BE THERE                                
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   SVDMWORK(96),DMWORK                                              
                                                                                
VR0340   DS    0H                                                               
         L     R6,AIO                                                           
         USING RSTRREC,R6                                                       
                                                                                
         TM    ACCFLAG,NEWPAGE                                                  
         BZ    VR0360                                                           
         MVC   RSTRKPG,PAGENUM                                                  
         DROP  R6                                                               
                                                                                
VR0360   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR0380                                                           
         MVI   ELCODE,RSTRACDQ                                                  
         GOTO1 REMELEM                                                          
                                                                                
VR0380   DS    0H                                                               
         LA    R2,ACCACCH                                                       
         MVI   SEQNUM,1                                                         
         LA    R4,12               NUMBER OF LINES TO PROCESS                   
         NI    ACCFLAG,X'FF'-NOBLNKLN FLAG FOR INPUT                            
                                                                                
VR0400   DS    0H                                                               
         LR    RF,R2               1ST FIELD OF LINE TO CHECK FOR INPUT         
         LA    RE,L'LENFLD         THERE WILL BE 7 TOTAL TO CHECK               
                                                                                
VR0420   DS    0H                  ADD ELEMENT IF LINE HAS ENTRIES              
         CLI   5(RF),0                                                          
         BNE   VR0440                                                           
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCT   RE,VR0420                                                        
                                                                                
         LR    R2,RF               NO INPUT ON THIS LINE                        
         B     VR0620                                                           
                                                                                
VR0440   DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RSTRACCD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RSTRACDE,RSTRACDQ                                                
         MVI   RSTRAELN,RSTRAELQ                                                
         MVC   RSTRACSQ,SEQNUM                                                  
                                                                                
         CLI   5(R2),0             ACCOUNT                                      
         BE    MISSFLD                                                          
         GOTO1 VALIADV             VALIDATE ADVERTISER                          
         BNZ   INVLFLD                                                          
         MVC   RSTRACCT,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)            SKIP EXPANSION                               
         AR    R2,R0                                                            
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             OFFICE                                       
         BE    MISSFLD                                                          
         GOTO1 VALIOFF             VALIDATE OFFICE                              
         BNZ   INVLFLD                                                          
         MVC   RSTRAOFF,8(R2)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             PRIOR SHARE                                  
         BE    VR0480                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    VR0460                                                           
         GOTO1 AVALDATA,DMCB,(3,(R2)),(1,0)                                     
         BNZ   INVLFLD             VALUE NOT NUMERIC:  ERROR                    
         OI    4(R2),X'20'         VALIDATED                                    
VR0460   EQU   *                                                                
         MVC   RSTRAPSH,8(R2)                                                   
VR0480   EQU   *                                                                
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             PRIOR DOLLARS                                
         BE    VR0520                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    VR0500                                                           
         GOTO1 AVALDATA,DMCB,(12,(R2)),(2,0)                                    
         BNZ   INVLFLD             VALUE NOT NUMERIC:  ERROR                    
         OI    4(R2),X'20'         VALIDATED                                    
VR0500   EQU   *                                                                
         MVC   RSTRAPDO,8(R2)                                                   
VR0520   EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             SHARE GOAL                                   
         BE    VR0560                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    VR0540                                                           
         GOTO1 AVALDATA,DMCB,(3,(R2)),(3,0)                                     
         BNZ   INVLFLD             VALUE NOT NUMERIC:  ERROR                    
         OI    4(R2),X'20'         VALIDATED                                    
VR0540   EQU   *                                                                
         MVC   RSTRASGL,8(R2)                                                   
VR0560   EQU   *                                                                
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLI   5(R2),0             SHARE DOLLARS                                
         BE    VR0600                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    VR0580                                                           
         GOTO1 AVALDATA,DMCB,(12,(R2)),(4,0)                                    
         BNZ   INVLFLD             VALUE NOT NUMERIC:  ERROR                    
         OI    4(R2),X'20'         VALIDATED                                    
VR0580   EQU   *                                                                
         MVC   RSTRASDL,8(R2)                                                   
         DROP  R6                                                               
VR0600   EQU   *                                                                
                                                                                
         ZIC   R0,0(R2)            BUMP TO NEXT                                 
         AR    R2,R0                                                            
                                                                                
         OI    ACCFLAG,NOBLNKLN    SOME TEXT WAS FOUND                          
                                                                                
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR0620   DS    0H                                                               
         ZIC   RF,SEQNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         BCT   R4,VR0400                                                        
                                                                                
         LA    R2,ACCACCH          CHECK IF ANY INPUT AT ALL                    
         TM    ACCFLAG,NOBLNKLN                                                 
         BZ    MISSFLD             MUST HAVE AT LEAST ONE LINE OF INPUT         
                                                                                
VR0640   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR0660                                                           
         MVC   CONACT(8),=C'CHANGE  '                                           
         OI    CONACTH+6,X'80'     XMIT                                         
         B     VR0680                                                           
                                                                                
VR0660   DS    0H                  IF NEW PAGE                                  
         TM    ACCFLAG,NEWPAGE                                                  
         BZ    VR0700                                                           
                                                                                
VR0680   DS    0H                                                               
         MVI   DATAOVRD,C'Y'       SET 'DATA OVERRIDDEN/CHANGED'                
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
         CLI   DMCB+8,0                                                         
         BE    VR0720                                                           
         DC    H'0'                                                             
                                                                                
VR0700   DS    0H                                                               
         MVI   DATAOVRD,C'Y'       SET 'DATA OVERRIDDEN/CHANGED'                
         MVC   DMWORK(96),SVDMWORK                                              
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VR0720   DS    0H                                                               
         CLI   DATAOVRD,C'Y'       DATA OVERRIDDEN/CHANGED?                     
         BNE   DR                  NO  -                                        
         GOTO1 SETORIDE                                                         
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
                                                                                
         CLC   KEY(RSTRKPG-RSTRKEY),SELCKEY                                     
         BE    DK03                                                             
         CLI   ACTNUM,ACTSEL       INCASE WE ARE DISPLAYING A LIST              
         BNE   DK03                OF RECORDS                                   
         CLI   TWALACT,ACTSEL                                                   
         BNE   DK03                                                             
         MVC   PAGENUM,RSTRKPG     THIS GETS THE CORRECT PAGE                   
                                                                                
DK03     DS    0H                  IF WE ARE EDITING OTHER THAN PAGE 1          
         CLI   PFKEY,ADDLINE         REDISPLAY THE PAGE WE JUST                 
         BE    DK05                  ADD/DEL A LINE FROM                        
         CLI   PFKEY,DELLINE                                                    
         BNE   DK08                                                             
                                                                                
DK05     DS    0H                                                               
         MVC   RSTRKPG,PAGENUM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
DK08     DS    0H                                                               
         MVC   SELCKEY,KEY                                                      
         MVC   ACCKEY,KEY                                                       
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   ACCSTAT(4),RSTRKSTA                                              
         MVC   ACCSTAT+4(2),=C'-L'                                              
         CLI   RSTRKSTA+4,C'L'                                                  
         BE    DK09                                                             
         MVC   ACCSTAT+4(2),=C'-T'                                              
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    *+10                                                             
         MVC   ACCSTAT+5(1),RSTRKSTA+4                                          
DK09     EQU   *                                                                
         MVI   ACCSTATH+5,6        OUTPUT LENGTH                                
         CLI   ACCSTAT+3,C' '                                                   
         BNE   DK10                                                             
         MVC   ACCSTAT+3(3),ACCSTAT+4                                           
         MVI   ACCSTATH+5,5        OUTPUT LENGTH                                
                                                                                
DK10     OI    ACCSTATH+6,X'80'    XMIT                                         
         MVC   STRGROUP,RSTRKGRP                                                
         MVC   STRSTAT,ACCSTAT     SAVE IN CASE WE PF TO OTHER SCRNS            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         XC    ACCPERI,ACCPERI     CLEAR THE FIELD                              
         ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),RSTRKSTD(2)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,ACCPERI)                                 
                                                                                
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    DK20                                                             
         MVI   ACCPERI+6,C'-'                                                   
                                                                                
         ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),RSTRKEND(2)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,ACCPERI+7)                               
                                                                                
DK20     DS    0H                                                               
         OI    ACCPERIH+6,X'80'    XMIT                                         
         MVC   STRPERI,ACCPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,ACCSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
                                                                                
         MVC   ACCLOCA,WORK+10                                                  
         OI    ACCLOCAH+6,X'80'    XMIT                                         
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         NI    ACCFLAG,X'FF'-NEWPAGE                                            
         TWAXC ACCACCH,ACCLSDLH,PROT=Y  CLEAR SCREEN                            
                                                                                
* DISPLAY SHARE GOAL, LAST UPD, DESC                                            
         MVC   KEY,ACCKEY                                                       
         GOTO1 DISINFO,DMCB,ACCSGOLH,ACCLDATH,ACCDESCH                          
         MVC   KEY,ACCKEY                                                       
                                                                                
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKPG,1                                                        
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
DR20     DS    0H                  FIND TOTAL PAGES IN THIS RECORD              
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BNE   DR30                                                             
         GOTO1 SEQ                                                              
         B     DR20                                                             
                                                                                
DR30     DS    0H                                                               
         LA    R6,KEYSAVE                                                       
         USING RSTRKEY,R6                                                       
         MVC   TOTPAGES,RSTRKPG                                                 
         DROP  R6                                                               
                                                                                
         MVI   ACCPAGE+3,C'/'                                                   
         LA    R2,ACCPAGEH                                                      
         EDIT  TOTPAGES,(3,12(R2)),FILL=0                                       
         OI    ACCPAGEH+6,X'80'    XMIT                                         
                                                                                
         MVC   KEY,ACCKEY                                                       
         CLI   ACTNUM,ACTADD       IF RECORD ADD, IGNORE PFKEYS                 
         BE    DR90                                                             
                                                                                
* PF 10 WAS HIT = PAGE UP                                                       
                                                                                
         CLI   PFKEY,PAGEUP                                                     
         BNE   DR40                                                             
         CLI   PAGENUM,1           ALREADY AT TOP OF PAGE                       
         BE    DR90                                                             
         ZIC   RF,PAGENUM                                                       
         BCTR  RF,0                                                             
         STC   RF,PAGENUM                                                       
         B     DR50                                                             
                                                                                
* PF 11 WAS HIT = PAGE DOWN                                                     
                                                                                
DR40     DS    0H                                                               
         CLI   PFKEY,PAGEDOWN                                                   
         BE    DR42                                                             
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE, WE NEED                 
         BE    DR50                TO FIND CURRENT PAGE TO REDISPLAY            
         B     DR90                                                             
                                                                                
DR42     DS    0H                                                               
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
         EDIT  PAGENUM,(3,ACCPAGE),FILL=0                                       
         OI    ACCFLAG,NEWPAGE                                                  
         B     DRX                                                              
                                                                                
DR45     DS    0H                                                               
         ZIC   RF,PAGENUM          STAY ON SAME PAGE                            
         BCTR  RF,0                                                             
         STC   RF,PAGENUM                                                       
                                                                                
DR50     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVC   RSTRKPG,PAGENUM                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
DR90     DS    0H                                                               
         EDIT  PAGENUM,(3,ACCPAGE),FILL=0                                       
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'        ACCOUNT/OFFICE DESCRIPTIVE ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   DR95                                                             
         USING RSTRA5CD,R6                                                      
         MVC   ACCCREA(18),=C'CREATED BY SEEDER,'                               
         TM    RSTRA5FL,X'40'      DATA OVERRIDDEN?                             
         BNO   DR92                NO                                           
         MVC   ACCCREA(18),=C'DATA OVERRIDDEN  ,'                               
DR92     EQU   *                                                                
         MVC   ACCCREA+19(06),=C'RANK ='                                        
         MVC   ACCCREA+26(12),=C'PRIOR ACTUAL'                                  
         TM    RSTRA5FL,X'20'      RANKED BY PRIOR ACTUAL?                      
         BO    DR95                YES                                          
         MVC   ACCCREA+26(12),=C'FORECAST    '                                  
         DROP  R6                                                               
         LA    R6,ACCCREAH                                                      
         OI    6(R6),X'80'         TRANSMIT THE FIELD                           
DR95     EQU   *                                                                
         L     R6,AIO                                                           
         USING RSTRACCD,R6                                                      
         MVI   ELCODE,RSTRACDQ     GOAL ACCOUNT/OFFICE                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE AT LEAST ONE ACCOUNT/OFF           
                                                                                
DR100    DS    0H                                                               
         MVI   SEQNUM,1                                                         
         LA    R2,ACCACCH          FIRST LINE OF OUTPUT TEXT                    
                                                                                
DR110    DS    0H                                                               
         CLC   SEQNUM,RSTRACSQ     TEXT BELONG ON THIS LINE?                    
         BE    DR130                                                            
                                                                                
         LA    R5,L'LENFLD                                                      
DR120    ZIC   R0,0(R2)            NO, BUMP TO NEXT LINE                        
         AR    R2,R0                                                            
         BCT   R5,DR120                                                         
         B     DR140                                                            
                                                                                
DR130    DS    0H                  YES, DISPLAY THIS LINE                       
         MVC   8(L'RSTRACCT,R2),RSTRACCT                                        
         OI    6(R2),X'80'         XMIT                                         
                                                                                
         GOTO1 VALIADV                                                          
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(20,R2),WORK                                                    
         OI    6(R2),X'80'         XMIT                                         
                                                                                
DR135    DS    0H                                                               
         ZIC   R0,0(R2)            ADVERTISER EXPANSION                         
         AR    R2,R0                                                            
                                                                                
         MVC   8(L'RSTRAOFF,R2),RSTRAOFF                                        
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         LA    R4,RSTRAPSH                                                      
         GOTO1 ALINDATA,DMCB,(3,(R4))                                           
         MVC   8(L'RSTRAPSH,R2),RSTRAPSH                                        
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'20'         PREVIOUSLY VALID                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         LA    R4,RSTRAPDO                                                      
         GOTO1 ALINDATA,DMCB,(12,(R4))                                          
         MVC   8(L'RSTRAPDO,R2),RSTRAPDO                                        
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'20'         PREVIOUSLY VALID                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         LA    R4,RSTRASGL                                                      
         GOTO1 ALINDATA,DMCB,(3,(R4))                                           
         MVC   8(L'RSTRASGL,R2),RSTRASGL                                        
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'20'         PREVIOUSLY VALID                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         LA    R4,RSTRASDL                                                      
         GOTO1 ALINDATA,DMCB,(12,(R4))                                          
         MVC   8(L'RSTRASDL,R2),RSTRASDL                                        
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'20'         PREVIOUSLY VALID                             
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
         CLI   PFKEY,PAGEUP        DON'T MOVE CURSOR IF WE JUST PAGED           
         BE    DRXX                  UP/DOWN                                    
         CLI   PFKEY,PAGEDOWN                                                   
         BE    DRXX                                                             
         OC    ACURSOR,ACURSOR                                                  
         BZ    DRXX                                                             
         MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
                                                                                
DRXX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   ALINDATA:  CHANGE IN FORMAT NECESSITATES CONVERTING, ON THE FLY,            
*        ANY DATA ENTERED EARLIER, WHICH WAS LEFT-ALIGNED, TO A RIGHT-          
*        ALIGNED FORMAT FOR REDISPLAY.                                          
*                                                                               
ALINDATA NTR1                                                                   
         ZICM  R4,1(R1),3          RESET A(ENTRY TO BE REALIGNED)               
         ZIC   R5,0(R1)            LENGTH OF ENTRY                              
         LR    R0,R5               SET LOOP CONTROL                             
         AR    R4,R5               FIND LAST CHARACTER + 1                      
         BCTR  R4,0                BACK UP TO LAST CHARACTER                    
         LR    RF,R4               SAVE A(LAST POSITION OF ENTRY)               
ALIN0020 EQU   *                   FIND FIRST SPACE FIELD                       
         CLI   0(R4),C' '          NON-SPACE FIELD FOUND?                       
         BE    ALIN0040            NO                                           
         CLI   0(R4),X'00'         NON-SPACE FIELD FOUND?                       
         BNE   ALIN0080            YES - MOVE IT OVER                           
ALIN0040 EQU   *                   FIND FIRST SPACE FIELD                       
         BCTR  R4,0                NO  - BACK UP 1 POSITION                     
         BCT   R0,ALIN0020         GO BACK AND LOOK A PREVIOUS POSN             
         B     ALIN0200            ALL POSITIONS SPACED -                       
ALIN0080 EQU   *                                                                
         CR    R0,R5               WAS LAST POSITION NON-SPACE/ZERO?            
         BE    ALIN0200            NO  - STILL AT LAST POSITION                 
         MVC   0(1,RF),0(R4)       MOVE NON-SPACE TO END                        
         MVI   0(R4),C' '          CLEAR LEADING CHAR TO SPACE                  
         BCTR  R4,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,ALIN0080         GO BACK AND MOVE ANOTHER                     
ALIN0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AVALDATA:  VALIDATE THE CONTENTS OF THE FIELD.  DATA INPUT MUST             
*        BE NUMERIC.  AFTER VALIDATION, FIELD WILL BE RIGHT-JUST-               
*        IFIED FOR STORAGE                                                      
*        NON-NUMERIC FIELDS WILL RETURN NON-ZERO CC.                            
*                                                                               
*        P1   =   BYTE 1    = FIELD LENGTH                                      
*                 BYTES 2-4 = A(SCREEN FIELD HEADER)                            
AVALDATA NTR1                                                                   
         ZICM  R2,1(R1),3          SET A(ENTRY TO BE VALIDATED)                 
         ZIC   R0,5(R2)            SET LENGTH OF INPUT                          
         LA    R2,8(R2)            BUMP TO DATA                                 
         ZICM  R4,1(R1),3          RESET A(ENTRY TO BE REALIGNED)               
         LA    R4,8(R4)            SET TO DATA IN FIELD                         
         ZIC   R5,0(R1)            LENGTH OF ENTRY                              
         ZIC   RF,4(R1)            DATA ENTRY FLAG FOR DUMP TEST                
AVAL0010 EQU   *                                                                
         CLI   0(R2),C'0'          VALIDATE NUMERICS                            
         BL    AVAL0150            ERROR INPUT                                  
         CLI   0(R2),C'9'                                                       
         BH    AVAL0150            ERROR INPUT                                  
         LA    R2,1(R2)            BUMP TO NEXT POSITION                        
         BCT   R0,AVAL0010         GO BACK FOR NEXT, IF ANY                     
         LR    R0,R5               SET LOOP CONTROL FOR REALIGN                 
         AR    R4,R5               FIND LAST CHARACTER + 1                      
         BCTR  R4,0                BACK UP TO LAST CHARACTER                    
*                                                                               
*   TEST                                                                        
*        CH    RF,=H'2'                                                         
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         LR    RF,R4               SAVE A(LAST POSITION OF ENTRY)               
AVAL0020 EQU   *                   FIND FIRST SPACE FIELD                       
         CLI   0(R4),C' '          NON-SPACE FIELD FOUND?                       
         BE    AVAL0040            NO                                           
         CLI   0(R4),X'00'         NON-SPACE FIELD FOUND?                       
         BNE   AVAL0080            YES - MOVE IT OVER                           
AVAL0040 EQU   *                   FIND FIRST SPACE FIELD                       
         BCTR  R4,0                NO  - BACK UP 1 POSITION                     
         BCT   R0,AVAL0020         GO BACK AND LOOK A PREVIOUS POSN             
         B     AVAL0200            ALL POSITIONS SPACED -                       
AVAL0080 EQU   *                                                                
         CR    R0,R5               WAS LAST POSITION NON-SPACE/ZERO?            
         BE    AVAL0200            NO  - STILL AT LAST POSITION                 
         MVC   0(1,RF),0(R4)       MOVE NON-SPACE TO END                        
         MVI   0(R4),C' '          CLEAR LEADING CHAR TO SPACE                  
         BCTR  R4,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,AVAL0080         GO BACK AND MOVE ANOTHER                     
         B     AVAL0200            FINISHED                                     
AVAL0150 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     AVAL0300                                                         
AVAL0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO: OKAY                          
AVAL0300 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DELETE THE RECORD                                                             
***********************************************************************         
DELR     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKPG,1           PAGE 1 AND DOWN                              
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   DELX                                                             
                                                                                
DELR10   DS    0H                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RSTRREC,R6                                                       
         OI    RSTRCNTL,X'80'      MARK FOR DELETION                            
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
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BE    DELR10                                                           
                                                                                
DELX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE THE RECORD                                                            
***********************************************************************         
RESR     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKPG,1           PAGE 1 AND DOWN                              
         DROP  R6                                                               
                                                                                
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   RESX                                                             
                                                                                
RESR10   DS    0H                                                               
         OI    DMINBTS,X'08'       READ FOR DELETE                              
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RSTRREC,R6                                                       
         NI    RSTRCNTL,X'FF'-X'80'  RESTORE                                    
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
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BE    RESR10                                                           
                                                                                
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
*                                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRACCQ                                                
         MVC   RSTRKREP,AGENCY                                                  
*                                                                               
         CLI   ACLGRUPH+5,0        FILTER ON GROUP/SUB                          
         BE    LR08                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS GROUP/SUB               
         B     LR10                                                             
*                                                                               
LR08     DS    0H                                                               
         CLI   ACLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS GROUP/SUB               
         MVC   RSTRKSTA,STATION    YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         CLC   RSTRKEY(RSTRKGRP-RSTRKEY),ACCKEY                                 
         BNE   LRX                                                              
*                                                                               
         CLI   RSTRKPG,1           ONLY PAGE 1 RECORDS                          
         BNE   LRSEQ                                                            
*                                                                               
         CLI   ACLGRUPH+5,0        FILTER ON GRP/SUB                            
         BE    LR25                                                             
         CLI   ACLGRUPH+5,1        FILTER ON GRP/SUB                            
         BH    LR23                                                             
         CLC   GRPSGRP(1),RSTRKGRP                                              
         BNE   LRX                 WANT ONLY RECORDS WITH THIS GRP/SUB          
         B     LR25                                                             
*                                                                               
LR23     DS    0H                                                               
         CLC   GRPSGRP,RSTRKGRP                                                 
         BNE   LRX                 WANT ONLY RECORDS WITH THIS GRP/SUB          
*                                                                               
LR25     DS    0H                                                               
         CLI   ACLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   STATION,RSTRKSTA                                                 
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
                                                                                
LR30     DS    0H                                                               
         CLI   ACLPERIH+5,0        FILTER ON PERIOD?                            
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
                                                                                
         MVC   LACCGRUP,RSTRKGRP   GROUP                                        
                                                                                
         MVC   LACCSTAT(4),RSTRKSTA STATION CALL LETTER                         
         MVC   LACCSTAT+4(2),=C'-L'                                             
         CLI   RSTRKSTA+4,C'L'                                                  
         BE    LR70                                                             
         MVC   LACCSTAT+4(2),=C'-T'                                             
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    LR70                                                             
         MVC   LACCSTAT+5(1),RSTRKSTA+4                                         
                                                                                
LR70     ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),RSTRKSTD(2)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,LACCPERI)                                
*                                                                               
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    LR90                                                             
         MVI   LACCPERI+6,C'-'                                                  
*                                                                               
         ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),RSTRKEND(2)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,LACCPERI+7)                              
         DROP  R6                                                               
                                                                                
LR90     DS    0H                                                               
         L     R6,AIO                                                           
         USING RSTRACCD,R6                                                      
         MVI   ELCODE,RSTRACDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
                                                                                
         MVC   LACCCODE,RSTRACCT                                                
         MVC   LACCOFF,RSTRAOFF                                                 
         DROP  R6                                                               
                                                                                
LR100    DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   SETORIDE:  NEED TO CYCLE THROUGH SET OF RECORDS, AND SET OVER-              
*        RIDE FLAG IN X'50' ELEMENTS TO 'ON'                                    
*                                                                               
SETORIDE NTR1                                                                   
         LA    R6,ACCCREAH         SET OVERRIDE MESSAGE                         
         MVC   8(18,R6),=C'DATA OVERRIDDEN  ,'                                  
         OI    6(R6),X'80'         SET TRANSMIT                                 
         MVC   KEY,ACCKEY          RESET KEY                                    
         MVI   KEY+26,0            SET PAGENUM TO ZERO                          
         GOTO1 HIGH                RETRIEVE KEY                                 
         B     SETO0040                                                         
SETO0020 EQU   *                                                                
         GOTO1 SEQ                                                              
SETO0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME THROUGH DATES?                          
         BNE   SETO0200            NO  - FINISHED                               
         GOTO1 GETREC              YES - RETRIEVE THE RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'        ACCOUNT/OFFICE DESCRIPTIVE ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   SETO0200            NOT CREATED BY SEEDER - FINISHED             
         OI    RSTRA5FL-RSTRA5CD(R6),X'40'                                      
*                                  SET OVERRIDDEN DATA FLAG                     
         GOTO1 PUTREC              REWRITE THE RECORD                           
         B     SETO0020            GO BACK FOR NEXT                             
SETO0200 EQU   *                                                                
         XIT1                                                                   
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
RECNOTF  MVC   RERROR,=AL2(413)    RECORD NOT FOUND. PLS ADD                    
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
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
* TABLE FOR LENGTH OF FIELDS IN ONE ACCOUNT/OFFICE LINE                         
*                                                                               
LENFLD   DS    0XL7                                                             
         DC    AL1(L'ACCACC),AL1(L'ACCXACC),AL1(L'ACCOFF)                       
         DC    AL1(L'ACCPSHR),AL1(L'ACCPDOL),AL1(L'ACCSHGL)                     
         DC    AL1(L'ACCSDOL)                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESTRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRF9D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRFAD          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESTRWORKD                                                     
       ++INCLUDE RESTRDSECT                                                     
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
         ORG   SYSSPARE                                                         
ADDLINE  EQU   2                                                                
DELLINE  EQU   3                                                                
PAGEUP   EQU   10                                                               
PAGEDOWN EQU   11                                                               
STARTDT  DS    XL2                                                              
ENDDT    DS    XL2                                                              
SVKEY    DS    CL(L'KEY)                                                        
SELCKEY  DS    CL(L'KEY)                                                        
ACCKEY   DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
SAVER6   DS    F                                                                
SVDMWORK DS    12D                                                              
AINSERT  DS    A                   A(ADDRESS OF INSERTION)                      
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SEQNUM   DS    X                                                                
GRPSGRP  DS    CL2                                                              
STATION  DS    CL5                                                              
                                                                                
ACCFLAG  DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
NEWPAGE  EQU   X'04'               USER HAS PAGED DOWN A NEW PAGE               
                                                                                
PAGENUM  DS    X                   CURRENT PAGE NUMBER                          
TOTPAGES DS    X                   TOTAL PAGES                                  
DATAOVRD DS    CL1                 DATA OVERRIDDEN FLAG                         
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LACCGRUP DS    CL2                                                              
         DS    CL2                                                              
LACCSTAT DS    CL7                                                              
         DS    CL2                                                              
LACCPERI DS    CL13                                                             
         DS    CL2                                                              
LACCCODE DS    CL4                                                              
         DS    CL5                                                              
LACCOFF  DS    CL2                                                              
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053RESTR05S  10/10/96'                                      
         END                                                                    
