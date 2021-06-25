*          DATA SET RESFM27    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T81827A                                                                  
         TITLE 'T81827 - RESFM27 - CONTRACT LABEL RECORD'                       
***********************************************************************         
*                                                                     *         
*  RESFM27 (T81827) --- MAINTENANCE/LIST OF CONTRACT LABEL RECORDS    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 16FEB93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 13JUL93 (SKU) CHECK FOR NUMERIC INPUT ONLY IN PACK ROUTINE          *         
*                                                                     *         
* 09AUG94 (SKU) PREVENT BLANK LABELS                                  *         
*                                                                     *         
* 22APR96 (RHV) SUPPORT 34 BYTE AGY ADDR FIELDS                       *         
*                                                                     *         
*HERE******************************************************************         
T81827   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81827*,R7,RR=R3                                              
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
         CLI   MODE,PRINTREP       PRINT RECORDS?                               
         BE    LR                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK10                                                             
         TM    LBLNAMEH+4,X'20'                                                 
         BNZ   VKX                                                              
         MVI   PREVFLAG,C'N'                                                    
         OI    LBLNAMEH+4,X'20'                                                 
         XC    KEY,KEY                                                          
         B     VKX                                                              
*                                                                               
         LA    R2,LABNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VK10     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RLABKEY,R4                                                       
         MVI   RLABTYP,RLABTYPQ                                                 
         MVC   RLABREP,AGENCY                                                   
         MVC   RLABNAME,LABNAME                                                 
         OC    RLABNAME,SPACES     SPACE PAD THE LABEL NAME                     
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,RLABDCDQ     REMOVE OLD DESCRIPTION ELEMENT               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,RLABFCDQ     REMOVE OLD LABEL FIELD DEFINITIONS           
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RLABDESD,R6                                                      
         XC    ELEM,ELEM                                                        
*                                                                               
         MVI   RLABDCDE,RLABDCDQ   DESCRIPTION ELEMENT CODE                     
         MVI   RLABDELN,RLABDOV    ELEMENT LENGTH                               
*                                                                               
         LA    R2,LABDESCH                                                      
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         ZIC   R1,5(R2)            MOVE IN LABEL DESCRIPTION                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RLABDESC(0),8(R2)                                                
*                                                                               
         ZIC   R0,RLABDELN         MOVE IN TOTAL LENGTH (VARIABLE)              
         ZIC   R1,5(R2)            FOR THIS ELEMENT                             
         AR    R0,R1               TOTAL LEN=OVERHEAD+LEN(DESC)                 
         STC   R0,RLABDELN                                                      
*                                                                               
VR20     DS    0H                                                               
         LA    R2,LABCOLH          LABEL WIDTH                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 PACK                                                             
         STC   R0,RLABDCOL                                                      
         CLI   RLABDCOL,MAXWIDTH   CHECK RANGE                                  
         BH    INVWRAN                                                          
         CLI   RLABDCOL,MINWIDTH                                                
         BL    INVWRAN                                                          
         MVC   UDMAXCOL,RLABDCOL   USER DEFINED MAX WIDTH                       
*                                                                               
         LA    R2,LABROWH          LABEL LENGTH                                 
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 PACK                                                             
         STC   R0,RLABDROW                                                      
         CLI   RLABDROW,MAXLEN     CHECK RANGE                                  
         BH    INVLRAN                                                          
         CLI   RLABDROW,MINLEN                                                  
         BL    INVLRAN                                                          
         MVC   UDMAXROW,RLABDROW   USER DEFINED MAX LENGTH                      
*                                                                               
         LA    R2,LABNUMH          NUMBER OF LABELS ACROSS PAGE                 
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 PACK                                                             
         STC   R0,RLABDNUM                                                      
         CLI   RLABDNUM,MAXNUM     CHECK RANGE                                  
         BH    INVXRAN                                                          
         CLI   RLABDNUM,MINNUM                                                  
         BL    INVXRAN                                                          
*                                                                               
         ZIC   RE,RLABDNUM         (WIDTH * #ACROSS) + #ACROSS <= 133           
         ZIC   R3,UDMAXCOL         ABOVE MUST BE TRUE                           
         MR    R2,RE               OTHERWISE CANNOT FIT ON ONE LINE!            
         LA    R2,LABNUMH          RESTORE HEADER                               
         AR    R3,RE                                                            
         CH    R3,=H'133'                                                       
         BH    INVXPAGE            ERROR, EXCEED A PRINT LINE                   
*                                                                               
         LA    R2,LABLINEH         LINES TO SKIP                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 PACK                                                             
         STC   R0,RLABDLNE                                                      
         CLC   RLABDLNE,RLABDROW   CHECK RANGE                                  
         BH    INVXLNE                                                          
         MVC   UDLINESK,RLABDLNE   USER DEFINED LINES TO SKIP                   
*                                                                               
         LA    R2,LABTESTH         NUMBER OF TEST PATTERN LABEL TO PRT          
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 PACK                                                             
         STC   R0,RLABDPAT                                                      
         CLI   RLABDPAT,MAXPAT     CHECK RANGE                                  
         BH    INVXPAT                                                          
         CLI   RLABDPAT,MINPAT                                                  
         BL    INVXPAT                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM             ADD NEW DESCRIPTION ELEMENT                  
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* CONTRACT LABEL ELEMENTS                                                       
*                                                                               
         LA    R5,ELEM                                                          
         USING RLABFDEF,R5                                                      
*                                                                               
         MVI   ISBLANK,C'Y'        LABEL IS BLANK                               
         LA    R2,LABSROWH                                                      
         LA    R3,CODETAB                                                       
*                                                                               
VR30     DS    0H                  ADD FIELD DEFINITION ELEMENT                 
         XC    ELEM,ELEM                                                        
         MVI   RLABFCDE,RLABFCDQ   ELEMENT CODE                                 
         MVI   RLABFELN,RLABFOV+1   ELEMENT LENGTH                              
*                                                                               
         STCM  R2,15,SAVER2        IF ERROR, POINT TO THIS FIELD                
*                                                                               
         CLI   5(R2),0             CHECK IF ROW MISSING                         
         BNE   VR40                                                             
         LR    R1,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R1,R0                                                            
         CLI   5(R1),0                                                          
         BNE   MISSFLD             ROW MISSING                                  
*                                                                               
         ZIC   R0,0(R2)            BOTH ROW AND COL MISSING, SKIP THIS          
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR210                                                            
*                                                                               
VR40     DS    0H                  CHECK IF COL MISSING                         
         LR    R1,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R1,R0                                                            
         CLI   5(R1),0                                                          
         BNE   VR50                                                             
         LR    R2,R1                                                            
         B     MISSFLD             COL MISSING                                  
*                                                                               
VR50     DS    0H                                                               
         MVI   ISBLANK,C'N'        LABEL IS NOT BLANK                           
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         STC   R0,RLABFROW         GET START ROW                                
         ZIC   RF,UDMAXROW         CHECK IF STRT ROW < MAXROW+LINE SKIP         
         ZIC   RE,UDLINESK                                                      
         AR    RE,R0                                                            
         CLI   0(R3),C'C'          AGY ADDRESS FIELD IS 2 ROWS LONG             
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BH    INVLFLD                                                          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         STC   R0,RLABFCOL         GET START COLUMN                             
         CLC   RLABFCOL,UDMAXCOL                                                
         BH    INVLFLD                                                          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R3),C'*'          IS THIS A LITERAL FIELD?                     
         BNE   VR65                                                             
*                                                                               
VR60     DS    0H                  YES,                                         
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         ZIC   RE,RLABFCOL         CHECK AGAINST LABEL WIDTH                    
         ZIC   RF,5(R2)                                                         
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         ZIC   RF,UDMAXCOL                                                      
         CR    RE,RF                                                            
         BH    INVLWDT             ERROR, EXCEEDS LABEL WIDTH                   
*                                                                               
         ZIC   R1,5(R2)            MOVE IN LITERAL                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RLABFNAM(0),8(R2)                                                
*                                                                               
         LA    R0,RLABFOV          MOVE IN TOTAL LENGTH (VARIABLE)              
         ZIC   R1,5(R2)            FOR THIS ELEMENT                             
         AR    R0,R1               TOTAL LEN=OVERHEAD+LEN(LIT)                  
         STC   R0,RLABFELN                                                      
         B     VR70                                                             
*                                                                               
VR65     DS    0H                                                               
         MVC   RLABFNAM,0(R3)      MOVE IN CODE LETTER                          
*                                                                               
         CLI   5(R2),0             IF NO LENGTH SPECIFIED                       
         BNE   VR68                                                             
         MVC   RLABFLEN,2(R3)      USE DEFAULT                                  
         B     VR69                                                             
*                                                                               
VR68     DS    0H                                                               
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    INVLLEN                                                          
         STC   R0,RLABFLEN         GET FIELD LENGTH                             
         CLC   RLABFLEN,2(R3)                                                   
         BH    INVLLEN             INVALID LENGTH                               
*                                                                               
VR69     DS    0H                  CHECK IF FIELD LENGTH WITHIN LABEL           
         ZIC   RE,RLABFCOL                                                      
         ZIC   RF,RLABFLEN                                                      
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         ZIC   RF,UDMAXCOL                                                      
         CR    RE,RF                                                            
         BH    INVLWDT             ERROR, EXCEEDS LABEL WIDTH                   
*                                                                               
VR70     DS    0H                  CHECK IF FIELDS OVERLAP                      
         L     R6,AIO                                                           
         MVI   ELCODE,RLABFCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VR200                                                            
*                                                                               
VR80     DS    0H                                                               
         CLC   RLABFROW,2(R6)      SKIP IF NOT SAME ROW                         
         BE    VR85                                                             
         CLC   =C'C',5(R6)         AGY ADDRESS FIELD                            
         BNE   VR82                                                             
         ZIC   R1,2(R6)            YES - CHECK FOLLOWING LINE ALSO              
         LA    R1,1(R1)                                                         
         CLM   R1,1,RLABFROW                                                    
         BE    VR85                                                             
VR82     DS    0H                                                               
         CLC   RLABFNAM,=C'C'      AGY ADDRESS FIELD?                           
         BNE   VR100               NO                                           
         ZIC   R1,RLABFROW         YES - CHECK NEXT LINE ALSO                   
         LA    R1,1(R1)                                                         
         CLM   R1,1,2(R6)                                                       
         BE    VR85                                                             
         B     VR100                                                            
VR85     CLC   RLABFCOL,3(R6)                                                   
         BE    INVLOVL             ERROR, FIELDS OVERLAP                        
         BL    VR90                                                             
*                                                                               
         ZIC   RE,3(R6)            SAME ROW, THIS FIELD STARTS LATER            
         ZIC   RF,4(R6)            SEE IF PREVIOUS FIELD + LENGTH WILL          
         AR    RE,RF               OVERLAP THIS FIELD                           
         ZIC   RF,RLABFCOL                                                      
         CR    RE,RF                                                            
         BNL   INVLOVL             ERROR, FIELDS OVERLAP                        
         B     VR100                                                            
*                                                                               
VR90     DS    0H                  SAME ROW, THIS FIELD STARTS SOONER           
         ZIC   RE,RLABFCOL         SEE IF THIS FIELD + LENGTH WILL              
         ZIC   RF,RLABFLEN         OVERLAP NEXT FIELD                           
         AR    RE,RF                                                            
         ZIC   RF,3(R6)                                                         
         CR    RE,RF                                                            
         BNL   INVLOVL             ERROR, FIELDS OVERLAP                        
*                                                                               
VR100    DS    0H                  CHECK ALL OTHERS ADDED BEFORE THIS           
         BAS   RE,NEXTEL                                                        
         BE    VR80                                                             
         DROP  R5                                                               
*                                                                               
VR200    DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR210                                                            
         DC    H'0'                                                             
*                                                                               
VR210    DS    0H                  BUMP TO NEXT FIELD                           
         LA    R3,L'CODETAB(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BE    VR220                                                            
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR30                                                             
*                                                                               
VR220    DS    0H                  PREVENT BLANK LABEL DEFINITION               
         CLI   ISBLANK,C'N'                                                     
         BE    VRX                                                              
         LA    R2,LABSROWH                                                      
         B     INVBLANK                                                         
                                                                                
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,C'Y'                                                    
         MVC   PREVKEY,KEY         RETURN KEY AFTER LIST SELECT                 
*                                                                               
DK10     DS    0H                                                               
         L     R6,AIO                                                           
         USING RLABREC,R6                                                       
         MVC   LABNAME,RLABNAME    DISPLAY LABEL NAME                           
         OI    LABNAMEH+6,X'80'    XMIT FIELD                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC LABDESCH            CLEAR SCREEN                                 
*                                                                               
         LA    R2,LABLENH                                                       
         LA    R3,CODETAB                                                       
*                                                                               
DR05     DS    0H                  DISPLAY DEFAULT VALUES                       
         EDIT  (1,2(R3)),(2,8(R2)),ALIGN=LEFT                                   
         LA    R3,L'CODETAB(R3)                                                 
         CLI   0(R3),C'*'          STOP AT LITERAL                              
         BE    DR08                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT ROW                             
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR05                                                             
*                                                                               
DR08     DS    0H                                                               
         L     R6,AIO                                                           
         USING RLABREC,R6                                                       
*                                                                               
         CLI   RLABDELN,RLABDOV    ANY DESCRIPTION?                             
         BNH   DR10                                                             
         ZIC   R1,RLABDELN         YES, MOVE IT IN                              
         LA    R0,RLABDOV                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LABDESC(0),RLABDESC                                              
*                                                                               
DR10     DS    0H                  SHOW WIDTH, LENGTH AND # ACROSS PAGE         
         EDIT  RLABDCOL,(2,LABCOL),ALIGN=LEFT                                   
         EDIT  RLABDROW,(2,LABROW),ALIGN=LEFT                                   
         EDIT  RLABDNUM,(2,LABNUM),ALIGN=LEFT                                   
         EDIT  RLABDLNE,(2,LABLINE),ALIGN=LEFT,ZERO=NOBLANK                     
         EDIT  RLABDPAT,(2,LABTEST),ALIGN=LEFT,ZERO=NOBLANK                     
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RLABFCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         USING RLABFDEF,R6                                                      
         LA    R2,LABSROWH                                                      
         LA    R3,CODETAB                                                       
*                                                                               
DR20     DS    0H                                                               
         CLI   RLABFLEN,0          IF FREE FORM LITERAL, DISPLAY LATER          
         BE    DR30                                                             
         CLC   RLABFNAM,0(R3)      CHECK IF ELEMENT CODE FOR THIS FIELD         
         BE    DR40                                                             
DR30     BAS   RE,NEXTEL                                                        
         BE    DR20                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR100                                                            
*                                                                               
DR40     DS    0H                  START ROW                                    
         EDIT  RLABFROW,(2,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R0,0(R2)            START COLUMN                                 
         AR    R2,R0                                                            
         EDIT  RLABFCOL,(2,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R0,0(R2)            FIELD LENGHT OR LITERAL                      
         AR    R2,R0                                                            
         EDIT  RLABFLEN,(2,8(R2)),ALIGN=LEFT                                    
*                                                                               
DR100    DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RLABFCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         LA    R3,L'CODETAB(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BNE   DR20                                                             
*                                                                               
DR200    DS    0H                  NOW CHECK IF ANY LITERALS                    
         L     R6,AIO                                                           
         MVI   ELCODE,RLABFCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         LA    R2,LABCMT1H                                                      
*                                                                               
DR210    DS    0H                                                               
         CLI   RLABFLEN,0          LOOK FOR FREE FORM LITERAL ONLY              
         BE    DR220                                                            
DR215    BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         B     DR210                                                            
*                                                                               
DR220    DS    0H                  START ROW                                    
         EDIT  RLABFROW,(2,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R0,0(R2)            START COLUMN                                 
         AR    R2,R0                                                            
         EDIT  RLABFCOL,(2,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R0,0(R2)            LITERAL                                      
         AR    R2,R0                                                            
         ZIC   R1,RLABFELN         MOVE IT IN                                   
         LA    R0,RLABFOV                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RLABFNAM                                                 
*                                                                               
         LA    RF,LABLFLDH         LAST FIELD ENCOUNTERED?                      
         CR    RF,R2                                                            
         BNH   DRX                                                              
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR215                                                            
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR03                                                             
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LR03     DS    0H                                                               
         CLI   PREVFLAG,C'N'       PREVIOUS KEY EXAMINED?                       
         BE    LR05                                                             
         MVC   KEY,PREVKEY         USE SELECTED KEY                             
         MVI   PREVFLAG,C'N'                                                    
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'RLABKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RLABKEY,R6                                                       
         MVI   RLABTYP,RLABTYPQ                                                 
         MVC   RLABREP,AGENCY                                                   
         MVC   RLABNAME,LBLNAME                                                 
         OC    RLABNAME,SPACES     SPACE PADDED                                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RLABKEY,R6                                                       
         CLI   RLABTYP,RLABTYPQ    MATCHING REC TYPE AND REP CODE               
         BNE   LRX                                                              
         CLC   RLABREP,AGENCY                                                   
         BNE   LRX                                                              
         DROP  R6                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RLABREC,R6                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   LLBNAME,RLABNAME    LABEL NAME                                   
*                                                                               
         MVI   LLBCOLX,C'W'        LABEL DESCRIPTION                            
         EDIT  RLABDCOL,(2,LLBCOL),FILL=0                                       
         MVI   LLBROWX,C'L'                                                     
         EDIT  RLABDROW,(2,LLBROW),FILL=0                                       
         MVI   LLBNUMX,C'X'                                                     
         EDIT  RLABDNUM,(2,LLBNUM),FILL=0                                       
*                                                                               
         CLI   RLABDELN,RLABDOV    ANY DESCRIPTION?                             
         BNH   LR40                                                             
         ZIC   R1,RLABDELN         YES, MOVE IT IN                              
         LA    R0,RLABDOV                                                       
         SR    R1,R0                                                            
         CH    R1,=H'47'           HAS ROOM FOR THIS MUCH                       
         BH    LR30                                                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LLBDESC(0),RLABDESC                                              
         B     LR40                                                             
*                                                                               
LR30     DS    0H                                                               
         MVC   LLBDESC,RLABDESC                                                 
         DROP  R6                                                               
*                                                                               
LR40     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRPRT                                                            
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRPRT    DS    0H                                                               
         MVC   P(LLBLEN),LISTAR                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ                                                            
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONVERT SCREEN INPUT TO PACK W/O SIGN                                         
***********************************************************************         
PACK     NTR1                                                                   
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BZ    INVLFLD                                                          
                                                                                
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*********************************************************************           
* HEADING FOR REPORT                                                            
*********************************************************************           
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,46,C'LABEL RECORDS'                                           
         SSPEC H2,46,C'-------------'                                           
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H8(L'HEADER),HEADER                                              
         MVC   H9(70),DASHES                                                    
         B     XIT                                                              
*                                                                               
DASHES   DC    70C'-'                                                           
HEADER   DC    CL21'NAME      DESCRIPTION'                                      
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
INVWRAN  MVC   RERROR,=AL2(371)                                                 
         B     ERREND                                                           
*                                                                               
INVLRAN  MVC   RERROR,=AL2(372)                                                 
         B     ERREND                                                           
*                                                                               
INVXRAN  MVC   RERROR,=AL2(373)                                                 
         B     ERREND                                                           
*                                                                               
INVXLNE  MVC   RERROR,=AL2(374)                                                 
         B     ERREND                                                           
*                                                                               
INVXPAT  MVC   RERROR,=AL2(375)                                                 
         B     ERREND                                                           
*                                                                               
INVLLEN  MVC   RERROR,=AL2(377)                                                 
         B     ERREND                                                           
*                                                                               
INVLWDT  MVC   RERROR,=AL2(385)                                                 
         B     ERREND                                                           
*                                                                               
INVBLANK MVC   RERROR,=AL2(393)                                                 
         B     ERREND                                                           
*                                                                               
INVLOVL  DS    0H                                                               
         ICM   R2,15,SAVER2                                                     
         MVC   RERROR,=AL2(378)                                                 
         B     ERREND                                                           
*                                                                               
INVXPAGE DS    0H                                                               
         MVC   RERROR,=AL2(379)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
MAXWIDTH EQU   70                  MAXIMUM WIDTH PER LABEL                      
MINWIDTH EQU   5                   MINIMUM WIDTH PER LABEL                      
MAXLEN   EQU   33                  MAXIMUM LENGTH PER LABEL                     
MINLEN   EQU   1                   MINIMUM LENGTH PER LABEL                     
MAXNUM   EQU   6                   MAXIMUM NUMBER OF LABELS ACROSS PAGE         
MINNUM   EQU   1                   MINIMUM NUMBER OF LABELS ACROSS PAGE         
MAXPAT   EQU   20                  MAXIMUM TEST PATTERNS TO PRINT               
MINPAT   EQU   0                   MINIMUM TEST PATTERNS TO PRINT               
UDMAXCOL DS    X                   USER DEFINED MAX WIDTH                       
UDMAXROW DS    X                   USER DEFINED MAX LENGTH                      
UDLINESK DS    X                   USER DEFINED LINES TO SKIP                   
SAVER2   DS    F                   SAVE POINTER TO STRT ROW OF EACH FLD         
*                                                                               
       ++INCLUDE REGENCDE                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCAD          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCBD          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENLAB                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENAGY                                                       
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENAGY2                                                      
         PRINT ON                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            APPLICATION SAVE STORAGE                     
PREVKEY  DS    CL(L'KEY)           FOR LIST TO RETURN TO                        
PREVFLAG DS    C                   (Y/N) PREVIOUS KEY USED INDICATOR            
ISBLANK  DS    C                   (Y/N) PREVENT BLANK LABELS                   
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LLBNAME  DS    CL8                 LABEL NAME                                   
         DS    CL2                                                              
LLBCOLX  DS    C                   W                                            
LLBCOL   DS    CL2                 WIDTH                                        
         DS    C                                                                
LLBROWX  DS    C                   L                                            
LLBROW   DS    CL2                 LENGTH                                       
         DS    C                                                                
LLBNUMX  DS    C                   X                                            
LLBNUM   DS    CL2                 NUMBER ACROSS PAGE                           
         DS    CL2                                                              
LLBDESC  DS    CL47                LABEL DESCRIPTION                            
LLBLEN   EQU   *-LLBNAME                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018RESFM27   05/01/02'                                      
         END                                                                    
