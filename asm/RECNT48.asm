*          DATA SET RECNT48    AT LEVEL 167 AS OF 06/21/00                      
*PHASE T80248A,+0                                                               
         TITLE 'T80248 - REP CF COMMENT DISPLAY/EDIT'                           
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT48 (T80248) --- CF COMMENT DISPLAY/EDIT             *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 19MAR/97 RHV  CREATION DATE                                     *             
* 30JUL/97 RHV  STAMP CFC REC WITH K VERSION #                    *             
*                                                                 *             
*                   ***  END TOMBSTONE  ***                       *             
*******************************************************************             
*                                                                               
T80248   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T80248,R9                                                
         LR    R7,RC                                                            
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
         DC    H'0'                                                             
*                                                                               
DISP     DS    0H                                                               
         BAS   RE,SETCOMBO         SETUP COMBO LIST                             
         BAS   RE,SETSCRN          SETUP SCREEN FIELDS & MESSAGES               
         BAS   RE,CLRSCRN          CLEAR SCREEN FIELDS                          
         BAS   RE,DISPCMT          DISPLAY CF COMMENTS                          
         B     EXXMOD                                                           
*                                                                               
EDIT     DS    0H                                                               
         BAS   RE,SETCOMBO         SETUP COMBO LIST                             
         BAS   RE,GETVER           GET K VERSION# FOR CFC REC                   
         BAS   RE,SAVECMT          SAVE COMMENT RECORD                          
         BAS   RE,CLRSCRN          CLEAR SCREEN FIELDS                          
         BAS   RE,DISPCMT          DISPLAY CF COMMENTS                          
         B     EXXMOD                                                           
*                                                                               
YES      CR    RB,RB                                                            
         B     EXXMOD                                                           
NO       LTR   RB,RB                                                            
         B     EXXMOD                                                           
*                                                                               
***********************************************************************         
* GETCMT - GET COMMENT RECORD (READS FOR DELETED RECORDS)             *         
*          CC ON RETURN: FALSE = RECORD NOT FOUND                     *         
*                        TRUE = RECORD FOUND                          *         
***********************************************************************         
GETCMT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCFCREC,R4                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,CMTCON                                                  
         OI    DMINBTS,X'08'       DELETES                                      
         MVC   UPDATE,CMTUPDTE                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         OI    DMINBTS,X'08'       DELETES                                      
         MVC   UPDATE,CMTUPDTE                                                  
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVI   CMTUPDTE,C'N'       RESET                                        
         B     YES                                                              
         DROP  R4                                                               
***********************************************************************         
* SAVECMT - WRITE COMMENT TEXT TO RECORD                              *         
***********************************************************************         
SAVECMT  NTR1                                                                   
*                                                                               
         BAS   RE,MSGCHGD          PUT OUT REC CHANGED MSG                      
*                                                                               
         NI    FLAGS,X'FF'-X'80'   RESET REC EXISTS FLAG                        
         NI    FLAGS,X'FF'-X'40'   RESET HAVE CMT LINE FLAG                     
         MVI   CMTUPDTE,C'Y'       PLAY FOR KEEPS                               
         BAS   RE,GETCMT                                                        
         BNE   *+8                                                              
         OI    FLAGS,X'80'         CMT REC EXISTS FLAG                          
*                                                                               
         LA    R6,IOAREA           BUILD FRESH CMT RECORD                       
         USING RCFCREC,R6                                                       
         XC    RCFCKEY(36),RCFCKEY                                              
         MVI   RCFCKTYP,RCFCKTYQ   REC TYPE                                     
         MVC   RCFCKREP,RCONKREP   REP                                          
         MVC   RCFCKCON,CMTCON     K NUM                                        
         MVI   RCFCLEN+1,36        DEFAULT LEN - EMPTY RECORD                   
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RCFCIEL,R6                                                       
         MVI   RCFCICD,X'01'       BUILD '01' INFO ELEMENT                      
         MVI   RCFCILEN,RCFCILNQ                                                
         MVC   RCFCIVER,CONVER                                                  
         GOTO1 VADDELEM,DMCB,IOAREA,ELEM                                        
         DROP  R6                                                               
*                                                                               
         LA    R2,CFCMGOH          MGO FIELD                                    
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    SAVE18              THEN WE CAN'T BE WRITING THIS OPTION         
         LA    R3,1                                                             
         CLI   5(R2),0             REQUIRED                                     
         BE    ERROR                                                            
         LA    R6,IOAREA                                                        
         USING RCFCREC,R6                                                       
         LA    R3,2                                                             
         CLI   8(R2),C'N'                                                       
         BE    SAVE15                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR                                                            
         OI    RCFCIFLG,X'80'                                                   
         B     SAVE18                                                           
SAVE15   OI    RCFCIFLG,X'40'                                                   
         DROP  R6                                                               
*                                                                               
SAVE18   DS    0H                                                               
         LA    R6,ELEM             ELEMENT BUILD AREA                           
         LA    R2,CFCCMT1H         FIRST LINE ON SCREEN                         
         LA    R3,CFCCMTXH         LAST LINE ON SCREEN                          
         USING RCFCTEL,R6                                                       
SAVE20   DS    0H                                                               
         CR    R2,R3               END OF SCREEN?                               
         BH    SAVE40              YES - DONE ADDING ELEMENTS                   
         CLI   5(R2),0             BLANK LINE?                                  
         BE    SAVE30              YES - NEXT LINE                              
*                                                                               
         OI    FLAGS,X'40'         WE HAVE AT LEAST 1 CMT LINE                  
         MVI   RCFCTCD,X'02'       BUILD '02' TEXT ELEMENT                      
         MVI   RCFCTLEN,RCFCTLNQ                                                
         MVC   RCFCTEXT,8(R2)                                                   
         GOTO1 VADDELEM,DMCB,IOAREA,ELEM                                        
*                                                                               
SAVE30   DS    0H                                                               
         LA    R2,CFCCMT2H-CFCCMT1H(R2) NEXT LINE ON SCREEN                     
         B     SAVE20                                                           
         DROP  R6                                                               
*                                                                               
SAVE40   DS    0H                                                               
         TM    FLAGS,X'80'         CMT REC ALREADY EXISTS?                      
         BZ    SAVE60              NO - NEED TO ADD                             
         TM    FLAGS,X'40'         HAVE AT LEAST 1 CMT LINE?                    
         BO    SAVE50              YES - DON'T DELETE RECORD                    
         OI    IOAREA+29,X'80'     HAVE NO CMTS - DELETE RECORD                 
SAVE50   DS    0H                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         MVC   KEY,IOAREA                                                       
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+27,X'FF'-X'80'  DEFAULT: KEY NOT DELETED                     
         TM    FLAGS,X'40'         HAVE AT LEAST 1 CMT LINE?                    
         BO    *+8                 YES - DON'T DELETE KEY                       
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE,DMCB,KEY                                                  
         B     EXXMOD                                                           
*                                                                               
SAVE60   DS    0H                                                               
         TM    FLAGS,X'40'         HAVE AT LEAST 1 CMT LINE?                    
         BZ    EXXMOD              NO - DON'T ADD RECORD                        
         GOTO1 VADDREC,DMCB,IOAREA                                              
         B     EXXMOD                                                           
***********************************************************************         
* GETVER - GETS MOST RECENT VERSION # FROM K RECORD, PUT IN 'CONVER'  *         
***********************************************************************         
GETVER   NTR1                                                                   
         MVI   CONVER,0            INITIALZE                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         USING RCONSEND,R6                                                      
         MVC   CONVER,RCONSRV                                                   
         CLC   RCONSRV,RCONSSV                                                  
         BH    EXXMOD                                                           
         MVC   CONVER,RCONSSV                                                   
         B     EXXMOD                                                           
***********************************************************************         
* SETCOMBO - BUFFER K NUMBERS IN COMBO, LOWEST K NUMBER IS CMTCON     *         
***********************************************************************         
SETCOMBO NTR1                                                                   
         XC    CBUFF,CBUFF                                                      
         MVC   CMTCON,RCONKCON                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    SCMB10                                                           
         MVC   CBUFF(4),RCONKCON                                                
         B     SCMBX                                                            
*                                                                               
SCMB10   DS    0H                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
         LA    R4,CBUFF            FIRST SPOT IN COMBO BUFFER                   
SCMB20   DS    0H                                                               
         MVC   0(4,R4),0(R5)       PUT K NUM IN COMBO BUFFER                    
         CLC   CMTCON,0(R4)        CMTCON VS. CURRENT K?                        
         BL    *+10                CMTCON IS LOWER - SKIP                       
         MVC   CMTCON,0(R4)        CMTCON IS HIGHER - REPLACE W/CURRENT         
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         LA    R4,4(R4)            NEXT SPOT IN COMBO BUFFER                    
         BCT   R3,SCMB20                                                        
*                                                                               
SCMBX    DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
* DISPCMT - DISPLAY CF COMMENTS                                      *          
***********************************************************************         
DISPCMT  NTR1                                                                   
*                                                                               
         MVI   CMTUPDTE,C'N'       DON'T READ FOR UPDATE                        
         BAS   RE,GETCMT           GET CMT RECORD                               
         BNE   DISCX               NOT FOUND                                    
         TM    IOAREA+29,X'80'     DELETED?                                     
         BO    DISCX                                                            
*                                                                               
         LA    R6,IOAREA                                                        
         LA    R2,CFCCMT1H                                                      
         LA    R1,CFCCMTXH                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISC20                                                           
         USING RCFCTEL,R6                                                       
         B     DISC15                                                           
DISC10   BAS   RE,NEXTEL                                                        
         BNE   DISC20                                                           
         LA    R2,CFCCMT2H-CFCCMT1H(R2)                                         
         CR    R2,R1                                                            
         BH    DISC20                                                           
DISC15   DS    0H                                                               
         MVC   8(L'CFCCMT1,R2),RCFCTEXT                                         
         B     DISC10                                                           
         DROP  R6                                                               
DISC20   DS    0H                                                               
         LA    R2,CFCMSGH          MGO FIELD                                    
         LA    R4,CFCMGOH          MGO MESSAGE FIELD                            
         LA    R6,IOAREA                                                        
         USING RCFCREC,R6                                                       
         TM    RCFCIFLG,X'80'      MGO TO FOLLOW (YES)?                         
         BZ    DISC30                                                           
         MVC   8(L'MGOYMSG,R2),MGOYMSG                                          
         MVI   5(R2),L'MGOYMSG                                                  
         MVI   8(R4),C'Y'                                                       
         MVI   5(R4),1                                                          
         B     DISC35                                                           
DISC30   DS    0H                                                               
         TM    RCFCIFLG,X'40'      MGO TO FOLLOW (NO)?                          
         BZ    DISCX                                                            
         MVC   8(L'MGONMSG,R2),MGONMSG                                          
         MVI   5(R2),L'MGONMSG                                                  
         MVI   8(R4),C'N'                                                       
         MVI   5(R4),1                                                          
DISC35   OI    6(R2),X'80'                                                      
         OI    6(R4),X'80'                                                      
DISCX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
***********************************************************************         
* SETSCRN - PROTECT/UNPROTECT APPROPRITE SCREEN FIELDS                *         
***********************************************************************         
SETSCRN  NTR1                                                                   
*                                                                               
         NI    FLAGS,X'FF'-X'20'   DEFAULT = EDIT COMMENTS                      
*                                                                               
         LA    R6,RCONREC          CHECK CONFIRMED STATUS                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   SETS05                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'80'      CONFIRMED NOW?                               
         BO    SETS05              NO - OK                                      
         OI    FLAGS,X'20'         YES - PROTECT CMTS                           
         B     SETS20                                                           
         DROP  R6                                                               
*                                                                               
SETS05   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'        SEND ELEM                                    
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         OI    FLAGS,X'20'         NO ELEM - PROTECT COMMENTS                   
         B     SETS20                                                           
         USING RCONSEND,R6                                                      
*                                                                               
*                                  ** STATION CASE **                           
         CLI   TWAACCS,C'$'        STATION SIGNED ON?                           
         BNE   SETS15              NO - GO TO REP CASE                          
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BO    *+8                                                              
         OI    FLAGS,X'20'                                                      
         TM    RCONSENF,X'10'+X'20'    VER # NOT ADVANCED?                      
         BO    *+8                                                              
         OI    FLAGS,X'20'                                                      
         B     SETS20                                                           
*                                                                               
SETS15   DS    0H                  ** REP CASE **                               
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BO    SETS17              YES                                          
         TM    RCONMODR+1,X'40'    REP USING GRAPHNET?                          
         BO    *+8                                                              
         OI    FLAGS,X'20'                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BO    *+8                                                              
         OI    FLAGS,X'20'                                                      
SETS17   TM    RCONSENF,X'10'+X'20'    VER # NOT ADVANCED?                      
         BO    *+8                                                              
         OI    FLAGS,X'20'                                                      
         DROP  R6                                                               
*                                                                               
SETS20   DS    0H                                                               
         TM    FLAGS,X'20'         PROTECT COMMENT LINES?                       
         BZ    SETS40              NO - SKIP THIS                               
*                                                                               
         LA    R2,CFCCMT1H         **PROTECT CMT LINES**                        
         LA    R1,CFCCMTXH                                                      
         B     *+8                                                              
SETS25   LA    R2,CFCCMT2H-CFCCMT1H(R2) NEXT LINE                               
         TM    1(R2),X'20'         ALREADY PROTECTED?                           
         BO    SETS30              YES - NEXT FIELD                             
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         XMIT                                         
SETS30   CR    R2,R1                                                            
         BL    SETS25                                                           
*                                                                               
         LA    R2,CFCMGOH          PROTECT MGO OPTION                           
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         BAS   RE,MSGDISP          DISPLAY 'CMTS DISPLAYED' MESSAGE             
*                                                                               
         LA    R2,CONBACTH                                                      
         XC    8(4,R2),8(R2)       CLEAR BUYACT FLD                             
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VAL BUYACT                     
         OI    6(R2),X'80'+X'40'   XMIT + CURSOR                                
         B     SETS50                                                           
*                                                                               
*                                  **ALLOW EDIT CMT LINES**                     
SETS40   DS    0H                                                               
         BAS   RE,MSGEDIT          DISPLAY 'EDIT CMTS' MESSAGE                  
         LA    R2,CONBACTH                                                      
         MVC   8(4,R2),=C'PCF '    KEEP BUYACT 'CFC'                            
         OI    4(R2),X'20'         PREV VAL BUYACT                              
         OI    6(R2),X'80'+X'40'   XMIT + CURSOR                                
*                                                                               
SETS50   DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
* CLRSCRN - CLEAR SCREEN FIELDS                                                 
***********************************************************************         
CLRSCRN  NTR1                                                                   
         LA    R2,CFCCMT1H         CLEAR COMMENT LINES                          
         LA    R1,CFCCMTXH                                                      
         B     *+8                                                              
CLR10    LA    R2,CFCCMT2H-CFCCMT1H(R2) NEXT LINE                               
         XC    8(L'CFCCMT1,R2),8(R2) CLEAR LINE                                 
         OI    6(R2),X'80'         XMIT                                         
         CR    R2,R1                                                            
         BL    CLR10                                                            
*                                                                               
         LA    R2,CFCMGOH          CLEAR MGO FIELD                              
         MVI   8(R2),C' '                                                       
         OI    6(R2),X'80'                                                      
         LA    R2,CFCMSGH          CLEAR MGO MSG FIELD                          
         XC    8(L'CFCMSG,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
*                                                                               
         B     EXXMOD                                                           
***********************************************************************         
* MESSAGE DISPLAY ROUTINES                                                      
***********************************************************************         
MSGEDIT  MVC   HALF,=H'164'                                                     
         B     MSG10                                                            
MSGDISP  MVC   HALF,=H'166'                                                     
         B     MSG10                                                            
MSGCHGD  MVC   HALF,=H'165'                                                     
         B     MSG10                                                            
*                                                                               
MSG10    DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),HALF                                                   
         ST    RE,FULL                                                          
         GOTO1 VDISMSG,DMCB,,                                                   
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
       ++INCLUDE RECFCMSG                                                       
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD9D                                                       
WORKD    DSECT                                                                  
CBUFF    DS    CL16                COMBO BUFFER (4 K NUMBERS)                   
CMTCON   DS    CL4                 K IN COMBO THAT GETS THE COMMENT             
CONVER   DS    CL1                 CONTRACT VERSION#                            
CMTUPDTE DS    C                   READ COMMENT RECORD FOR UPDATE (Y/N)         
FLAGS    DS    X                   PROGRAM FLAGS                                
*                                  X'80' COMMENT RECORD EXISTS                  
*                                  X'40' AT LEAST 1 CMT LINE (EDIT)             
*                                  X'20' COMMENTS PROTECTED                     
ELEM     DS    CL62                CMT ELEM BUILD AREA                          
WORKLNQ  EQU   *-WORKD                                                          
       ++INCLUDE REGENCFC                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'167RECNT48   06/21/00'                                      
         END                                                                    
