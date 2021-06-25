*          DATA SET RESFM22    AT LEVEL 024 AS OF 05/01/02                      
*PHASE T81822A,*                                                                
*INCLUDE DAYPAK                                                                 
*INCLUDE OUTDAY                                                                 
         TITLE 'T81822 - RESFM22 - RDA ADD/CHANGE/DISPLAY/LIST/REPORT'          
*********************************************************************           
*                                                                   *           
*  RESFM22 (T81822) --- RDA RECORD ADD/CHANGE/DISPLAY/LIST/REPORT   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 12MAR91  (EFJ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* 23JUL91  (EFJ) --- MAKE SURE TIME RANGE IS GIVEN (MORE THAN       *           
*                    ONE TIME).                                     *           
*                                                                   *           
* 21AUG91  (EFJ) --- FIX LENGTH BUG                                 *           
*                                                                   *           
* 26AUG91  (EFJ) --- MAKE ERROR MESSAGE NUMBER SOFT                 *           
*                                                                   *           
* 25MAR93  (SKU) --- ADD CHECK FOR END OF B'CAST DAY                *           
*                    SUPPORT REP PROFILE FOR 6A-559A B'CAST DAY     *           
*                                                                   *           
*HERE****************************************************************           
*                                                                               
T81822   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1822**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
         CLI   MODE,XRECPUT                                                     
         BE    DREC                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VKEY     DS    0H                                                               
         LA    R2,RDACDEH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VKXIT                                                            
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      NOT REQUIRED FOR LIST                        
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BNE   ERREND                                                           
VK10     OI    4(R2),X'20'         MARK FIELD PRE-VALID                         
* BUILD KEY                                                                     
VKXIT    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RDARECD,R4                                                       
         MVI   RRDAKTYP,X'33'                                                   
         MVC   RRDAKREP,AGENCY                                                  
         MVC   RRDAKCDE,RDACDE                                                  
         OC    RRDAKCDE,SPACES                                                  
VKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     DS    0H                                                               
         L     R4,AIO                                                           
         USING RDARECD,R4                                                       
         XC    LIN,LIN                                                          
         XC    INSADR,INSADR                                                    
         XC    NUMINS,NUMINS                                                    
         LA    R5,INSADR                                                        
* COPY OF REC                                                                   
         L     R0,AIO1             'FROM' ADDRESS                               
         LA    R1,1000             'FROM' LENGTH                                
         L     RE,AIO2             'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
*                                                                               
* VALIDATE NAME FIELD                                                           
         LA    R2,RDANAMH                                                       
         TM    4(R2),X'20'         PRE-VALID?                                   
         BNZ   VR10                                                             
         GOTO1 ANY                                                              
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RRDANMEL,R6                                                      
         MVC   RRDANMCD(2),=X'012A'                                             
         MVC   RRDANMNM,WORK                                                    
         XC    RRDANMCT,RRDANMCT                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
* VALIDATE DAYPARTS                                                             
VR10     DS    0H                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM             DELETE OLD X'02' ELEMS                       
         LA    R2,RDATXTH          FIRST DAYPART                                
*                                                                               
* START OF ELEM                                                                 
VR20     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RRDADPEL,R6                                                      
         MVC   RRDADPCD(2),=X'0226'                                             
*                                                                               
* VALIDATE TEXT (MANDATORY)  (NOTE: R2 SHOULD POINT AT TEXT FLD)                
         CLI   5(R2),0                                                          
         BNE   VR30                                                             
         SR    R0,R0                                                            
         MVC   RERROR,=AL2(IAFTBLK)                                             
*                                                                               
* IF TEXT IS BLANK, CAN BE NO INPUT FOLLOWING                                   
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    VRXIT                                                            
         CLI   5(R2),0                                                          
         BNE   ERREND2                                                          
         B     *-22                                                             
*                                                                               
VR30     DS    0H                  COUNT LINES ON SCREEN                        
         ZIC   R1,LIN                                                           
         LA    R1,1(R1)                                                         
         STC   R1,LIN                                                           
*                                                                               
         CLC   =C'@D',8(R2)        DELETE THIS LINE?                            
         BNE   VR40                                                             
*                                                                               
* DELETE THIS LINE BY NOT ADDING IT (POSN R2 TO NEXT LINE)                      
         SR    R0,R0                                                            
         LA    R1,5                                                             
*                                                                               
         IC    R0,0(R2)            R2 TO START OF NEXT LINE                     
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
         B     VR20                                                             
*                                                                               
VR40     DS    0H                                                               
         CLC   =C'@I',8(R2)        INSERT A BLANK LINE?                         
         BNE   VR50                                                             
*                                                                               
* KEEP COUNT OF NUMBER OF LINES INSERTED (SO WE KNOW WHERE TO                   
* POSN BLANK LINE)                                                              
         MVC   RERROR,=AL2(MANYLINE)                                            
         ZIC   R1,NUMINS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NUMINS                                                        
         CLI   NUMINS,7                                                         
         BH    ERREND2                                                          
         LR    R3,R2                                                            
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    *+12                                                             
*                                                                               
         LA    R3,RDATXT2H-RDATXTH(R3)                                          
         BCT   R1,*-4                                                           
*                                                                               
         ST    R3,0(R5)            STORE ADDRESS WHERE TO INS ON SCREEN         
         LA    R5,4(R5)                                                         
         BAS   RE,REDISP           REDISPLAY CURRENT LINE FROM REC              
*                                                                               
VR50     DS    0H                                                               
         MVC   RERROR,=AL2(MANYENT)  LIMIT OF 9 REPORTABLE ENTRIES              
         MVC   RRDADPTX,8(R2)                                                   
         OC    RRDADPTX,SPACES                                                  
         CLC   =C'+       ',RRDADPTX  '+' IS NOT AN ENTRY, BUT A CONT           
         BE    VR60                                                             
*                                                                               
         ZIC   R1,RRDANMCT         INC COUNTER IN X'01' ELEM                    
         LA    R1,1(R1)                                                         
         STC   R1,RRDANMCT                                                      
         CLI   RRDANMCT,RRDAMXCT   MORE THAN MAX ENTRIES?                       
         BH    ERREND2                                                          
*                                                                               
* VALIDATE DAY/TIME 1 (MANDATORY)                                               
VR60     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 ANY                                                              
*                                                                               
         CLI   WORK,C'*'           ALL OTHERS?                                  
         BNE   VR80                                                             
         SR    R0,R0                                                            
         MVC   RERROR,=AL2(IAFTAST)                                             
*                                                                               
* IF * IS USED, CAN BE NO INPUT FOLLOWING                                       
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    VR70                                                             
         CLI   5(R2),0                                                          
         BNE   ERREND2                                                          
         B     *-22                                                             
*                                                                               
VR70     GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELEM,=C'ADD=CODE'              
         B     VRXIT                                                            
*                                                                               
* REALLY ENTERED A D/T                                                          
VR80     MVI   ERROR,INVALID                                                    
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK),C',=/='                               
         CLI   DMCB+4,2            TWO LINES?                                   
         BNE   ERREND                                                           
*                                                                               
* GOTO DAYPAK TO VALIDATE DAYS                                                  
         MVI   ERROR,INVDAY                                                     
         LA    R3,WORK                                                          
         GOTO1 =V(DAYPAK),DMCB,(0(R3),12(R3)),RRDADPD1+0,RRDADPD1+1,   X        
               RR=RELO                                                          
         CLI   RRDADPD1+0,0                                                     
         BE    ERREND                                                           
*                                                                               
* GOTO TIMVAL TO VALIDATE TIMES                                                 
         MVI   ERROR,INVTIME                                                    
         LA    R3,WORK+32          2ND SCANNER BLOCK                            
         GOTO1 TIMVAL,DMCB,(0(R3),12(R3)),RRDADPD1+2                            
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         OC    RRDADPD1+4(2),RRDADPD1+4   CAN'T BE SINGLE TIME                  
         BZ    ERREND                                                           
*                                                                               
         MVC   STARTTM,=H'0500'    START TIME 5A                                
         CLI   CSTRT6AM,C'Y'       START/END TIME PROFILE TO 6A-559A?           
         BNE   VR83                (SET IN REP PROFILE)                         
         MVC   STARTTM,=H'0600'    YES, SET START TIME TO 6A                    
*                                                                               
VR83     DS    0H                                                               
         ZICM  R0,RRDADPD1+2,2     START TIME                                   
         CH    R0,STARTTM                                                       
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
*                                                                               
         ZICM  R1,RRDADPD1+4,2     END TIME                                     
         CH    R1,STARTTM                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
*                                                                               
         CR    R0,R1               START TIME MUST BE ON OR BEFORE              
         BH    ERREND                END TIME                                   
*                                                                               
* VALIDATE TIME ZONE 1 (OPTIONAL)                                               
*                                                                               
VR85     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         CLI   8(R2),C'E'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'C'                                                       
         BNE   ERREND                                                           
         MVC   RRDADPT1,8(R2)                                                   
*                                                                               
* VALIDATE DAY/TIME 2 (OPTIONAL)                                                
VR90     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   VR100                                                            
*                                                                               
* MAKE SURE TIME ZONE 2 IS EMPTY IF NO DAY/TIME 2                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
         B     VR110                                                            
*                                                                               
VR100    DS    0H                                                               
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK),C',=/='                               
         CLI   DMCB+4,2            TWO LINES?                                   
         BNE   ERREND                                                           
*                                                                               
* GOTO DAYPAK TO VALIDATE DAYS                                                  
         MVI   ERROR,INVDAY                                                     
         LA    R3,WORK                                                          
         GOTO1 =V(DAYPAK),DMCB,(0(R3),12(R3)),RRDADPD2+0,RRDADPD2+1,   X        
               RR=RELO                                                          
         CLI   RRDADPD2+0,0                                                     
         BE    ERREND                                                           
*                                                                               
* GOTO TIMVAL TO VALIDATE TIMES                                                 
         MVI   ERROR,INVTIME                                                    
         LA    R3,WORK+32          SECOND SCANNER BLOCK                         
         GOTO1 TIMVAL,DMCB,(0(R3),12(R3)),RRDADPD2+2                            
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         OC    RRDADPD2+4(2),RRDADPD2+4   CAN'T BE SINGLE TIME                  
         BZ    ERREND                                                           
*                                                                               
         MVC   STARTTM,=H'0500'    START TIME 5A                                
         CLI   CSTRT6AM,C'Y'       START/END TIME PROFILE TO 6A-559A?           
         BNE   VR105               (SET IN REP PROFILE)                         
         MVC   STARTTM,=H'0600'    YES, SET START TIME TO 6A                    
*                                                                               
VR105    DS    0H                                                               
         ZICM  R0,RRDADPD2+2,2     START TIME                                   
         CH    R0,STARTTM                                                       
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
*                                                                               
         ZICM  R1,RRDADPD2+4,2     END TIME                                     
         CH    R1,STARTTM                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
*                                                                               
         CR    R0,R1               START TIME MUST BE ON OR BEFORE              
         BH    ERREND                END TIME                                   
*                                                                               
* VALIDATE TIME ZONE 2 (OPTIONAL)                                               
*                                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         CLI   8(R2),C'E'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'C'                                                       
         BNE   ERREND                                                           
         MVC   RRDADPT2,8(R2)                                                   
*                                                                               
*                                                                               
VR110    GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELEM,=C'ADD=CODE'              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR20                                                             
*                                                                               
* MAKE SURE AT LEAST 1 DAYPART ELEM                                             
VRXIT    DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,RDATXTH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERREND                                                           
VRXX     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DKEY     DS    0H                                                               
         L     R4,AIO                                                           
         USING RDARECD,R4                                                       
*                                                                               
         LA    R2,RDACDEH                                                       
         MVC   8(L'RRDAKCDE,R2),RRDAKCDE                                        
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
DKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         XC    INSERR,INSERR       INSERT ERROR FLAG                            
         LA    R5,INSADR                                                        
*                                                                               
* DISPLAY NAME                                                                  
         L     R6,AIO                                                           
         USING RDARECD,R6                                                       
         MVC   RDANAM,RRDANMNM                                                  
         OI    RDANAMH+6,X'80'     SET XMIT BIT                                 
         DROP  R6                                                               
*                                                                               
DR5      LA    R2,RDATXTH                                                       
         SR    R1,R1                                                            
DR10     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    DR20                END OF SCREEN                                
         TM    1(R2),X'20'                                                      
         BNZ   DR20                NO PROTECTED FLDS IN DATA AREA               
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         XMIT FLD                                     
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR10                                                             
*                                                                               
* DISPLAY DAYPARTS                                                              
DR20     LA    R2,RDATXTH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING RRDADPEL,R6                                                      
DR30     DS    0H                                                               
         CLI   INSERR,1            WAS AN ERROR FOUND?                          
         BE    DR35                                                             
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BZ    *+12                                                             
         MVI   INSERR,1                                                         
         B     DR5                                                              
*                                                                               
         CLI   MODE,XRECPUT                                                     
         BNE   DR35                                                             
         CLM   R2,7,1(R5)          IS THIS ADDR IN TABLE?                       
         BNE   DR35                                                             
*                                                                               
* LEAVE BLANK LINE FOR INSERT                                                   
         LA    R5,4(R5)            NEXT INSADR                                  
         SR    R0,R0                                                            
         LA    R1,5                                                             
*                                                                               
         IC    R0,0(R2)            R2 TO START OF NEXT LINE                     
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
         B     DR30                                                             
*                                                                               
DR35     DS    0H                                                               
         MVC   8(L'RRDADPTX,R2),RRDADPTX                                        
*                                                                               
* DISPLAY DAY/TIME 1                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(24,R2),SPACES                                                  
         OC    RRDADPD1,RRDADPD1                                                
         BNZ   DR40                                                             
         MVI   8(R2),C'*'                                                       
         B     DRXIT               NOTHING CAN FOLLOW '*'                       
*                                                                               
* LET OUTDAY DISPLAY DAYS                                                       
DR40     GOTO1 =V(OUTDAY),DMCB,RRDADPD1+0,RRDADPD1+1,(0,8(R2)),RR=RELO          
         LA    R3,8+11(R2)         OUTDAY OUTPUT CAN BE UP TO 11 CHARS          
         CLI   0(R3),C' '          LAST CHAR OF TIME?                           
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
* LET UNTIME DISPLAY TIMES                                                      
         GOTO1 UNTIME,DMCB,RRDADPD1+2,(R3)                                      
*                                                                               
* DISPLAY TIME ZONE 1 (IF PRESENT)                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    RRDADPT1,RRDADPT1                                                
         BZ    DR50                                                             
         MVC   8(L'RRDADPT1,R2),RRDADPT1                                        
*                                                                               
* DISPLAY DAY/TIME 2 (IF PRESENT)                                               
DR50     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    RRDADPD2,RRDADPD2                                                
         BNZ   DR60                IF NO D/T2, THEN NO TZ2                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR70                                                             
*                                                                               
* LET OUTDAY DISPLAY DAYS                                                       
DR60     MVC   8(24,R2),SPACES                                                  
         GOTO1 =V(OUTDAY),DMCB,RRDADPD2+0,RRDADPD2+1,(0,8(R2)),RR=RELO          
         LA    R3,8+11(R2)         OUTDAY OUTPUT CAN BE UP TO 11 CHARS          
         CLI   0(R3),C' '          LAST CHAR OF TIME?                           
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
* LET UNTIME DISPLAY TIMES                                                      
         GOTO1 UNTIME,DMCB,RRDADPD2+2,(R3)                                      
*                                                                               
* DISPLAY TIME ZONE 2 (IF PRESENT)                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    RRDADPT2,RRDADPT2                                                
         BZ    DR70                                                             
         MVC   8(L'RRDADPT2,R2),RRDADPT2                                        
*                                                                               
DR70     DS    0H                                                               
         ZIC   R0,0(R2)            R2 TO NEXT 'TEXT' FIELD                      
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR30                                                             
*                                                                               
DRXIT    DS    0H                                                               
         CLI   INSERR,1            WAS AN ERROR FOUND?                          
         BNE   DRXX                                                             
*                                                                               
         LA    R2,RDATXTH                                                       
         MVC   RERROR,=AL2(MANYLINE)                                            
         MVI   RTXTLEN,2                                                        
         LA    RE,=C'14'                                                        
         STCM  RE,7,RTXTADR                                                     
         B     ERREND2                                                          
DRXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
LR5      LA    R4,KEY                                                           
         USING RRDAKEY,R4                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THRU                         
         BNZ   LR7                                                              
         MVI   RRDAKTYP,X'33'                                                   
         MVC   RRDAKREP,AGENCY                                                  
         MVC   RRDAKCDE,RDACDE                                                  
         OC    RRDAKCDE,SPACES                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR7      GOTO1 HIGH                                                             
         B     LR15                                                             
         DROP  R4                                                               
*                                                                               
LR10     GOTO1 SEQ                                                              
LR15     CLC   KEY(19),SAVEKEY      CORRECT REP                                 
         BNE   LRXX                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RDARECD,R4                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
*                                                                               
         MVC   LTXT,RRDAKCDE                                                    
         MVC   LNAME,RRDANMNM                                                   
*                                                                               
LR100    CLI   MODE,PRINTREP                                                    
         BNE   LR150                                                            
         MVC   P(L'LISTAR),LISTAR  FOR REPORT                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE 1                                                                
LR150    GOTO1 LISTMON             FOR LIST                                     
         SPACE 1                                                                
LR200    B     LR10                NEXT RECORD                                  
         SPACE 1                                                                
LRXX     B     XIT                                                              
         EJECT                                                                  
****************************************************************                
* REDISPLAY ELEM CHANGED BY @I                                 *                
****************************************************************                
         SPACE 3                                                                
REDISP   NTR1                                                                   
         L     R6,AIO2             COPY OF OLD REC                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIN,1                                                            
         BE    RD10                                                             
         ZIC   R3,LIN                                                           
         BCTR  R3,0                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         BAS   RE,NEXTEL                                                        
         BNE   ERREND                                                           
         BCT   R3,*-8                                                           
*                                                                               
         USING RRDADPEL,R6                                                      
RD10     MVC   8(8,R2),RRDADPTX                                                 
         OC    8(8,R2),SPACES                                                   
RDX      B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,47,C'RDA RECORDS'                                             
         SSPEC H2,47,C'-----------'                                             
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         MVC   H8(L'HEADING),HEADING                                            
         MVC   H9(29),DASHES                                                    
         B     XIT                                                              
         EJECT                                                                  
         SPACE 3                                                                
DASHES   DC    80C'-'                                                           
HEADING  DC    CL13'CODE     NAME'                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
ERREND   GOTO1 ERREX               ERROR NUM IN 'ERROR'  (1 BYTE)               
ERREND2  GOTO1 MYERROR             ERROR NUM IN 'RERROR' (2 BYTE)               
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LTXT     DS    CL8                                                              
         DS    CL1                                                              
LNAME    DS    CL20                                                             
         DS    CL51                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMD8D                                                                      
* RESFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD7D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
SAVEKEY  DS    CL27                                                             
INSADR   DS    7F                  SCRN ADDR WHICH TO INSERT BLNK LN            
NUMINS   DS    X                   NUMBER OF LINES INSERTED                     
LIN      DS    X                   DISPLAY LINE LOOKING AT                      
INSERR   DS    X                   FLAG FOR INSERT ERROR                        
STARTTM  DS    H                   START TIME                                   
*                                                                               
RDARECD  DSECT                                                                  
       ++INCLUDE REGENRDA                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024RESFM22   05/01/02'                                      
         END                                                                    
