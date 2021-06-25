*          DATA SET RECNT39    AT LEVEL 053 AS OF 10/08/15                      
*PHASE T80239A                                                                  
         TITLE 'MULTIPLE CHANGE INPUT - EDIT - T80239'                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT39 (T80239) --- MULTIPLE CHANGE INPUT (MCI)             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
* 08Oct15 KWA  Bypass AM validation                                   *         
* 11APR03 SKU  FIX END DATE BUG                                       *         
* 13FEB01 RHV  LOCKOUT TARGET CANCEL BUYS                             *         
* 10JAN01 RHV  SPORTS BUY LOCKOUT                                     *         
* 19SEP00 RHV  FIX BUG INTRODUCED BY FIXING PREVIOUS BUG              *         
* 26MAY99 RHV  FIX RTS MAX SPOT BUCKET BUG                            *         
* 04JAN99 RHV  HANDLE NON-DELETED CANCELLED BUYS                      *         
* 17FEB98 RHV  FIX MCI CAN ERROR BUG                                  *         
* 05FEB98 JRD  ALT BUCKETS                                            *         
* 29SEP97 RHV  VOILA!                                                 *         
*                                                                     *         
***********************************************************************         
T80239   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80239,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R8,ASPULAR          USE SPULAR FOR WORKING STORAGE               
         USING WORKD,R8                                                         
***********************************************************************         
* MAIN PROGRAM                                                                  
***********************************************************************         
         CLC   =C'MCI',BUYACT      ONLY SUPPORT ACTIONS MCI/MCIR                
         BE    MAIN005                                                          
         CLC   =C'MCIR',CONACT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN005  DS    0H                                                               
         GOTO1 =A(INIT),RR=Y                                                    
*                                                                               
***>     BAS   RE,VALBUY           VALIDATE BUYLINE FIELD                       
***>     BAS   RE,VALOFLT          VALIDATE OLD FLT FIELD                       
***>     BAS   RE,CKBUYS           CHECK BUYS                                   
         GOTO1 =A(VALBUY),RR=Y                                                  
         GOTO1 =A(VALOFLT),RR=Y                                                 
         GOTO1 =A(CKBUYS),RR=Y                                                  
*                                                                               
         BAS   RE,VALFLT           VALIDATE NEW FLT NUM                         
         BAS   RE,VALDT            VALIDATE DAY/TIME FIELDS                     
         BAS   RE,VALLEN           VALIDATE LENGTH FIELD                        
         BAS   RE,VALDATE          VALIDATE DATE FIELD                          
         BAS   RE,VALRATE          VALIDATE RATE FIELDS                         
         BAS   RE,VALCMT           VALIDATE COMMENT                             
         BAS   RE,VALOCMT          VALIDATE ORDER COMMENT                       
*                                                                               
         CLI   MODE,0                                                           
         BNE   MAIN020                                                          
         TM    STAT,CANCEL                                                      
         BO    MAIN020                                                          
         LA    R2,MCIDAYSH                                                      
         LA    R3,745              MUST ENTER CHANGES TO MAKE                   
         B     ERROR                                                            
*                                                                               
MAIN020  DS    0H                                                               
*                                                                               
         LA    RF,CONNUM1       PREPARE TO LOOP THRU K'S                        
         STCM  RF,15,ACONNUM                                                    
         LA    RF,NEWRATE1                                                      
         STCM  RF,15,ARATE                                                      
         MVC   COMBOCNT,COMBONUM                                                
         B     *+8                                                              
*                                                                               
MAIN030  DS    0H                  BEGIN K LOOP                                 
         STC   RF,COMBOCNT                                                      
*                                                                               
         BAS   RE,READCON          READ IN CONTRACT REC                         
         BAS   RE,UPDBUYS          UPDATE BUYS                                  
         BAS   RE,UPDCON           COMPLETE K UPDATE, WITE K BACK               
*                                                                               
         ICM   RF,15,ACONNUM       NEXT K                                       
         LA    RF,4(RF)                                                         
         STCM  RF,15,ACONNUM                                                    
         ICM   RF,15,ARATE         NEXT RATE                                    
         LA    RF,4(RF)                                                         
         STCM  RF,15,ARATE                                                      
         ZIC   RF,COMBOCNT                                                      
         BCT   RF,MAIN030          LOOP                                         
*                                                                               
         GOTO1 =A(GENMSG),RR=Y                                                  
         B     XIT                                                              
*                                                                               
***********************************************************************         
* UPDCON - UPDATE CONTRACT RECORD                                               
***********************************************************************         
UPDCON   NTR1                                                                   
*                                                                               
* COMPLETE K UPDATES HERE                                                       
*                                                                               
         TM    STAT,DID1BUY        DID WE PROCESS A BUYLINE?                    
         BZ    XIT                 NO - NO REASON TO UPDATE K                   
*                                                                               
* UPDATE REP VERSION NUMBER AND UNCONFIRM IF NECESSARY                          
*                                                                               
CONUP    MVI   TAREQ,0                                                          
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    CONPUT                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    CONUP10                                                          
         DROP  R6                                                               
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   DUB(4),HELLO                                                     
         MVC   DUB+4(4),DATCON                                                  
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         GOTOX (RFGENVER,VREPFACS),DMCB,(C'R',RCONREC),DUB,(R7)                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONUP10  LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    *+12                                                             
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
         OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
*                                                                               
         MVC   CONMOD,MYSPACES     DISPLAY CON MOD NUMBER                       
         OI    CONMODH+6,X'80'                                                  
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET?                                
         BZ    CONUP40                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
*                                                                               
         TM    RCONCONF,X'40'      CONFIRMED - SHOW MOD NO.                     
         BO    CONUP40                                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLC   RCONSRV,RCONSSV                                                  
         BH    CONUP20                                                          
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     CONUP30                                                          
CONUP20  EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
CONUP30  TM    RCONMODR+1,X'40'    GRAPH?                                       
         BZ    *+14                                                             
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     *+10                                                             
         MVC   CONMOD(7),=C'WIP VER'                                            
         DROP  R6                                                               
         B     CONPUT                                                           
*                                                                               
CONUP40  CLI   RCONMOD,0                                                        
         BE    CONPUT                                                           
         MVC   CONMOD(7),=C'MOD NUM'                                            
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT,FLOAT=-                         
*                                                                               
CONPUT   OI    RCONMODR,X'40'      BUY CHANGE INDICATOR ON CON                  
***      CLI   MODNUM,0            WAS CON MOD NUMGER BUMPED                    
***      BE    CONPUT10            NO-SKIP TO CONTRACT PUT                      
***      OI    TAREQ,1             SET T/A REQ INDICATOR                        
***      NI    RCONMODR,X'5F'      NO CON HEADLINE CHANGE                       
***      MVC   RCONMOD,MODNUM      UPDATE CON MOD NUMBER                        
***      MVC   RCONMODD,TODAY      AND DATE                                     
*                                                                               
* PUT FURTHER CHANGES TO CONTRACT HERE                                          
*                                                                               
         TM    STAT,UPDCROSS                                                    
         BZ    *+8                                                              
         BAS   RE,UCROSS                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RCONKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
*  WITH CONTRACT RECORD REWRITTEN, DETERMINE IF AN 'EC' KEY NEEDS               
*     TO BE GENERATED.  IF SO, DO IT.                                           
*                                                                               
         TM    MODE,DODATE+DORATE                                               
         BNZ   CONUP50                                                          
         TM    STAT,CANCEL                                                      
         BZ    XIT                                                              
CONUP50  DS    0H                                                               
         GOTO1 =A(GENECKEY),RR=Y                                                
         B     XIT                                                              
********************************************************************            
* UCROSS - FOR BIAS ELECTRONIC CONTRACT, UPDATE CROSS DAY.                      
********************************************************************            
UCROSS   NTR1                                                                   
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   UCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
*                                                                               
         MVC   RCONCCCD,CROSSD     UPDATE NEW CROSS DAY                         
         B     XIT                                                              
         DROP  R6                                                               
                                                                                
UCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,CROSSD                                                        
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* READCON - READ CONTRACT REC AND SET BUCKET FLAGS                              
***********************************************************************         
READCON  NTR1                                                                   
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCONREC,R6                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,REPALPHA                                                
         ICM   RF,15,ACONNUM                                                    
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,0(RF)),(2,RCONPCON)                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
         TM    TWAFLAGS,X'08'      REP USING 'DAILY PACING'?                    
         BNO   RDCN0020            NO                                           
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
RDCN0020 EQU   *                                                                
         B     XIT                                                              
***********************************************************************         
* UPDBUYS - UPDATE BUYLINES                                                     
***********************************************************************         
UPDBUYS  NTR1                                                                   
         LA    R4,BUYLST                                                        
         ICM   R2,15,ACONNUM                                                    
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,0(R2)),(3,BUYCON)                    
*                                                                               
UPDB010  DS    0H                                                               
         CLI   0(R4),0             END OF BUY LIST                              
         BE    UPDBX                                                            
         CLI   0(R4),X'FF'                                                      
         BE    UPDBNEXT                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,BUYCON                                                  
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         MVC   RBUYKMLN,0(R4)                                                   
         MVC   RBUYKLIN,0(R4)                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         BAS   RE,UPDCAN           PERFORM CANCEL                               
*                                                                               
         BAS   RE,UPDFLT           UPDATE FLIGHT NUM                            
*                                                                               
         BAS   RE,UPDDT            UPDATE DAY/TIME                              
*                                                                               
         BAS   RE,UPDLEN           UPDATE LENGTH                                
*                                                                               
         BAS   RE,UPDDAT           UPDATE FLT DATE                              
         BNZ   UPDBERR                                                          
*                                                                               
         BAS   RE,UPDRATE          UPDATE RATE                                  
*                                                                               
         BAS   RE,UPDCMT           UPDATE COMMENT                               
*                                                                               
         BAS   RE,UPDOCM           UPDATE ORDER COMMENT                         
*                                                                               
**** MAKE BUYLINE ADJUSTMENTS HERE ****                                         
*                                                                               
         MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYKMOD,RCONMOD                                                 
*                                                                               
         TM    RCONMODR+1,X'C0'                                                 
         BZ    UPDB020             NO SEND ELEM, NO VERSION NOS                 
         SR    R1,R1                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         ZIC   R1,RCONSRV                                                       
         TM    RCONSENF,X'20'      REP VER ADVANCED?                            
         BZ    UPDB015             YES                                          
         LA    R1,2(R1)            NO - BUMP VER NUM                            
         ZIC   RF,RCONSSV                                                       
         CR    R1,RF               VS. STA VERSION                              
         BNL   UPDB015             OK                                           
         LR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         DROP  R6                                                               
UPDB015  DS    0H                                                               
         ZIC   RF,RBUYVER                                                       
         CR    R1,RF               SAME VER AS LAST CHANGE                      
         BE    *+10                                                             
         MVC   RBUYCHGI(2),=C'  '  NO - CLEAR CHG INDICATORS                    
         STC   R1,RBUYVER                                                       
UPDB020  DS    0H                                                               
*                                                                               
* SET BUY CHANGE INDICATORS                                                     
*                                                                               
UPDB050  DS    0H                                                               
         TM    STAT,CANCEL                                                      
         BZ    UPDB060                                                          
         MVC   RBUYCHGI(2),=C'C '                                               
         B     UPDB100                                                          
UPDB060  DS    0H                                                               
         MVC   HALF,RBUYCHGI                                                    
         TM    RCONMODR+1,X'C0'                                                 
         BZ    UPDB080             NO SEND ELEM, NO VERSION NOS                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLC   RBUYVER,RCONSRV     SAME VER, DON'T RESET CHG INDICATOR          
         BE    UPDB080                                                          
         XC    HALF,HALF                                                        
         DROP  R6                                                               
UPDB080  DS    0H                                                               
         TM    MODE,DODAY                                                       
         BZ    *+12                                                             
         MVI   BYTE,C'D'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DODATE                                                      
         BZ    *+12                                                             
         MVI   BYTE,C'E'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DOFLT                                                       
         BZ    *+12                                                             
         MVI   BYTE,C'F'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DOOCMT                                                      
         BZ    *+12                                                             
         MVI   BYTE,C'O'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DOTIME                                                      
         BZ    *+12                                                             
         MVI   BYTE,C'T'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DOLEN                                                       
         BZ    *+12                                                             
         MVI   BYTE,C'L'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DORATE                                                      
         BZ    *+12                                                             
         MVI   BYTE,C'R'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         TM    MODE,DOCMT                                                       
         BZ    *+12                                                             
         MVI   BYTE,C'Z'                                                        
         BAS   RE,CHGIND                                                        
*                                                                               
         MVC   RBUYCHGI,HALF                                                    
*                                                                               
UPDB100  DS    0H                  CHECK MAX 50 SPOTS ON REP-SPOT XFER          
         TM    STAT,CANCEL         CANCEL?                                      
         BO    UPDB135             YES - SKIP CHECK                             
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL            SPOTPAK INTERFACE ELEMENT?                   
         BNE   UPDB135             NO                                           
*                                                                               
*  MAKE DUMMY CALL TO GENBUC SO RBUYREC HAS ACCURATE TOTAL SPOTS                
         MVC   WORK(4),VGTBROAD                         A(GETBROAD)             
         MVC   WORK+4(4),GETDAY                         A(GETDAY)               
         MVC   WORK+8(4),ADDAY                          A(ADDAY)                
         MVC   WORK+12(4),DATCON                        A(DATCON)               
         MVC   WORK+16(4),DATAMGR                       A(DATAMGR)              
         MVC   WORK+20(4),VRECUP                        A(RECUP)                
         GOTO1 (RFGENBUC,VREPFACS),DMCB,RBUYREC,AIO4,WORK                       
         TM    RBUYCOMB,X'80'      IS THIS A 'NA' RATE BUY?                     
         BZ    UPDB125                                                          
*                                                                               
         SR    R0,R0               YES, CHECK FOR MAX 50 SPOTS                  
         NI    RBUYCOMB,X'FF'-X'80' CALULATE TOTAL SPOTS SINCE                  
         ZIC   R1,RBUYCOMB         NA RATE LINES DO NOT HAVE TOTAL SPTS         
         OI    RBUYCOMB,X'80'      CLEAR AND RESTORE COMBO FLAG                 
*                                  RBUYCOMB LESS HOB IS #SPOT                   
         ZIC   RE,RBUYTWKS                                                      
         MR    R0,RE               MULTIPLY R1(#SPT) & RE(TOTAL WKS)            
         B     UPDB130             R1 HAS PRODUCT(TOTAL SPTS)                   
*                                                                               
UPDB125  DS    0H                                                               
         LH    R1,RBUYTSPT                                                      
         LTR   R1,R1               IF NEGATIVE SKIP CHECK                       
         BM    UPDB135                                                          
UPDB130  DS    0H                                                               
         CH    R1,=H'169'          MAX OF 169 SPOTS                             
         BH    UPDBERR                                                          
*                                                                               
UPDB135  DS    0H                  RE-READ BUY TO SUBTRACT OLD BUCKETS          
         OI    DMINBTS,X'08'                                                    
         MVC   KEY(27),RBUYKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                  SUBTRACT OLD BUCKETS FROM K                  
         L     R0,VRECUP                                                        
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         LA    R5,TWABYTOT         CALCULATE A(WORKSPACE)                       
         AH    R5,=Y(TWABYTRD-TWABYTOT)                                         
         DROP  RF                                                               
*                                                                               
*   NOTE:  THIS IS IN 'TRADE BUCKET TOTAL' AREA IN THE TWA, WHICH               
*        IS NOT USED WITHIN THIS MODULE.                                        
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(X'FF',AIO4),                  +        
               (BUCKFLGS,RCONREC),ACOMFACS,VGTBROAD,(R0),(R5)                   
         BNE   UPDB140                                                          
*                                                                               
         TM    STAT,CANCEL         CANCEL?                                      
         BO    UPDB150             YES - DON'T REWRITE BUCKETS TO K             
*                                  UPDATE K BUCKETS                             
*                                                                               
         GOTO1 =A(BUCKUP),(R1),(R4),(RC),RR=Y                                   
*                                                                               
         BZ    UPDB150                                                          
*                                                                               
UPDB140  DS    0H                                                               
         LA    R2,CONBACTH                                                      
         L     R3,DUB              DUB CONTAINS ERROR                           
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
UPDB150  DS    0H                  WRITE BUY BACK TO FILE                       
         OI    DMINBTS,X'08'                                                    
         MVC   KEY(27),RBUYKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         OI    STAT,DID1BUY        HAVE PROCESSED AT LEAST 1 LINE               
         B     UPDBNEXT                                                         
*                                                                               
UPDBERR  DS    0H                  PUT CURRENT BUYNUM IN ERRLST                 
         LA    RF,ERRLST                                                        
         B     *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   *-8                                                              
         MVC   0(1,RF),RBUYKLIN                                                 
         MVI   0(R4),X'FF'         DON'T PROCESS AGAIN                          
*                                                                               
UPDBNEXT LA    R4,1(R4)            NEXT BUY                                     
         B     UPDB010                                                          
*                                                                               
UPDBX    B     XIT                                                              
***********************************************************************         
* CHGIND - SET CHANGE INDICATOR                                                 
* TAKES CHANGE CODE IN 'BYTE', PUTS IT IN 'HALF'                                
***********************************************************************         
CHGIND   NTR1                                                                   
         OC    HALF,MYSPACES                                                    
         CLI   HALF,C'*'                                                        
         BE    XIT                                                              
         CLC   BYTE,HALF                                                        
         BE    XIT                                                              
         CLC   BYTE,HALF+1                                                      
         BE    XIT                                                              
         CLI   HALF,C' '                                                        
         BNE   *+14                                                             
         MVC   HALF(1),BYTE                                                     
         B     XIT                                                              
         CLI   HALF+1,C' '                                                      
         BNE   *+14                                                             
         MVC   HALF+1(1),BYTE                                                   
         B     XIT                                                              
         MVC   HALF,=C'* '                                                      
         B     XIT                                                              
***********************************************************************         
* UPDDAT - PERFORM FLIGHT DATE CHANGE                                           
***********************************************************************         
UPDDAT   NTR1                                                                   
         TM    MODE,DODATE                                                      
         BZ    XIT                 CC:0                                         
*                                                                               
         GOTO1 =A(EFFDAT),RR=Y                                                  
         LTR   R0,R0               TEST FOR ERROR                               
         BNZ   XIT                 CC:NOT 0                                     
*                                                                               
         CLI   WORK2,X'03'         HAVE AN '03' BUCKET?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK THAT MISSED AND/OR CREDIT ELEMENTS ARE CONSISTENT WITH NEW              
* X'03' ELEMENTS                                                                
*                                                                               
READ10   DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL            ANY MISSED ELEMENTS?                         
         BE    READ12              YES                                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'07'        CREDIT ELEMENT?                              
         BAS   RE,GETEL                                                         
         BE    READ12                                                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'56'        SPLIT MG REF ELEMENT?                        
         BAS   RE,GETEL                                                         
         BNE   READ10X                                                          
*                                                                               
READ12   BAS   RE,CKMISS           CHECK THAT ELEMENT IS COVERED BY NEW         
         LTR   R0,R0               X'03' ELS.                                   
         BP    XIT                 CC:NOT 0                                     
         SR    R1,R1                                                            
*                                                                               
READ13   IC    R1,1(R6)                                                         
         LR    R7,R6               SAVE PREVIOUS EL ADDR                        
         AR    R6,R1               POINT TO NEXT EL                             
         CLI   0(R6),6             IS IT AN 06 EL                               
         BE    *+12                YES                                          
         CLI   0(R6),7             NO-TRY FOR AN 07 EL                          
         BNE   READ10X                                                          
         CLC   2(3,R7),2(R6)       SKIP CHECK IF MISSED DATE                    
         BE    READ13              IS THE SAME AS LAST ONE                      
         B     READ12                                                           
         SPACE 2                                                                
* CHECK THAT MISSED MAKEGOOD OFFER ELTS ARE CONSISTENT WITH NEW                 
* X'03' ELEMENTS                                                                
*                                                                               
READ10X  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL            ANY MG OFFER ELEMENTS?                       
         BNE   READ14              NO  - GET OUT                                
*                                                                               
READ12X  BAS   RE,CKMISS2          CHECK THAT MG ELT IS COVERED BY NEW          
         LTR   R0,R0               X'03' ELS.                                   
         BP    XIT                 CC:NOT 0                                     
*                                                                               
READ13X  DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         LR    R7,R6               SAVE PREVIOUS EL ADDR                        
         AR    R6,R1               POINT TO NEXT EL                             
         CLI   0(R6),X'66'         IS IT A 66 EL                                
         BNE   READ14              NO                                           
         CLC   7(3,R7),7(R6)       SKIP CHECK IF MISSED DATE                    
         BE    READ13X             IS THE SAME AS LAST ONE                      
         B     READ12X                                                          
*                                                                               
READ14   DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(3,RBUYREC)                                        
         LA    R6,WORK2            NEW 03 BUCKETS                               
UDAT050  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,(R6)                                       
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   UDAT050                                                          
         B     XIT                 CC:0                                         
*                                                                               
* SUB-ROUTINE TO CHECK THAT MISSED OR CREDIT SPOT ELEMENT IS                    
* COVERED BY NEW EFFECTIVE DATES ELEMENTS. R6 POINTS TO ELEMENT.                
*                                                                               
CKMISS   NTR1                                                                   
         ZIC   R2,NUMFLTS          NUMBER OF X'03' ELEMENTS                     
         LA    R3,WORK2            POINT R3 TO AREA WITH ELEMENTS               
         USING RBUYDTEL,R3         AND COVER W DSECT.                           
*                                                                               
CK2      TM    RBUYDTIN,ALT        TEST FOR ALTERNATE WEEKS                     
         BO    CK4                 GO TO SPECIAL LOGIC                          
*                                                                               
         CLC   2(3,R6),RBUYDTST                                                 
         BL    CK10                TOO EARLY FOR THIS ELEMENT-TRY NEXT          
         CLC   2(3,R6),RBUYDTED                                                 
         BH    CK10                TOO LATE-TRY NEXT                            
         B     CKFND               FOUND-ELEMENT IS COVERED                     
*                                                                               
CK4      GOTO1 DATCON,DMCB2,(3,RBUYDTST),(0,EFFST)                              
         GOTO1 (RF),(R1),(3,RBUYDTED),(0,EFFEND)                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYELEM,R6                                                      
         ZIC   R5,RBUYSTED         INSERT START AND END DAY                     
         DROP  R6                                                               
*                                                                               
         SR    R5,R5               LOW ORDER BYTE OF R6 HAS                     
         SRDL  R4,4                START DAY                                    
         SRL   R5,28               R7 WILL HAVE END DAY                         
         CR    R4,R5               START V END DAY                              
         BNH   CK5                                                              
         LA    R5,7(R5)            DERIVE NUMBER OF DAYS THAT WHEN              
         SR    R5,R4               ADDED TO START DATE IN WEEK WILL             
         LR    R4,R5               GIVE END DATE                                
         B     CK6                                                              
*                                                                               
CK5      SR    R4,R5                                                            
*                                                                               
CK6      MVC   WORK(6),EFFST                                                    
         GOTO1 DATCON,(R1),(3,2(R6)),(0,DUB)                                    
*                                                                               
CK8      CLC   DUB(6),WORK         MISSED DATE V START DATE OF WEEK             
         BL    CK9                 TRY NEXT ACTIVE WEEK                         
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R4)                                      
         CLC   DUB(6),WORK+6                                                    
*                                  TRY NEXT ACTIVE WEEK                         
         BNH   CKFND               ITS COVERED                                  
*                                                                               
CK9      LA    R7,14               FIND NEXT ACTIVE WEEK                        
         GOTO1 ADDAY,(R1),WORK,EFFST,(R7)                                       
         CLC   EFFST,EFFEND                                                     
         BH    CK10                NOT IN THIS X'03' ELEMENT                    
         MVC   WORK(6),EFFST                                                    
         B     CK8                 COMPARE NEXT ACTIVE WEEK                     
*                                                                               
CK10     LA    R3,LTHRDEL(R3)      POINT TO NEXT X'03' ELEMENT                  
         BCT   R2,CK2                                                           
         LA    R0,MG1ERR           UNCOVERED 6 OR 7 ELEMENT                     
         B     CKEXIT                                                           
*                                                                               
CKFND    SR    R0,R0               COVERED ELEMENT                              
*                                                                               
CKEXIT   XIT1  REGS=(R0)                                                        
         DROP  R3                                                               
*                                                                               
* SUB-ROUTINE TO CHECK THAT MG MISSED ELEMENT IS                                
* COVERED BY NEW EFFECTIVE DATES ELEMENTS. R6 POINTS TO ELEMENT.                
*                                                                               
CKMISS2  NTR1                                                                   
         ZIC   R2,NUMFLTS          NUMBER OF X'03' ELEMENTS                     
         LA    R3,WORK2            POINT R3 TO AREA WITH ELEMENTS               
         USING RBUYDTEL,R3         AND COVER W DSECT.                           
*                                                                               
CKK2     TM    RBUYDTIN,ALT        TEST FOR ALTERNATE WEEKS                     
         BO    CKK4                GO TO SPECIAL LOGIC                          
*                                                                               
         CLC   9(3,R6),RBUYDTST                                                 
         BL    CKK10               TOO EARLY FOR THIS ELEMENT-TRY NEXT          
         CLC   9(3,R6),RBUYDTED                                                 
         BH    CKK10               TOO LATE-TRY NEXT                            
         B     CKKFND              FOUND-ELEMENT IS COVERED                     
*                                                                               
CKK4     GOTO1 DATCON,DMCB2,(3,RBUYDTST),(0,EFFST)                              
         GOTO1 (RF),(R1),(3,RBUYDTED),(0,EFFEND)                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYELEM,R6                                                      
         ZIC   R5,RBUYSTED         INSERT START AND END DAY                     
         DROP  R6                                                               
*                                                                               
         SR    R5,R5               LOW ORDER BYTE OF R6 HAS                     
         SRDL  R4,4                START DAY                                    
         SRL   R5,28               R7 WILL HAVE END DAY                         
         CR    R4,R5               START V END DAY                              
         BNH   CKK5                                                             
         LA    R5,7(R5)            DERIVE NUMBER OF DAYS THAT WHEN              
         SR    R5,R4               ADDED TO START DATE IN WEEK WILL             
         LR    R4,R5               GIVE END DATE                                
         B     CKK6                                                             
*                                                                               
CKK5     SR    R4,R5                                                            
*                                                                               
CKK6     MVC   WORK(6),EFFST                                                    
         GOTO1 DATCON,(R1),(3,2(R6)),(0,DUB)                                    
*                                                                               
CKK8     CLC   DUB(6),WORK         MISSED DATE V START DATE OF WEEK             
         BL    CKK9                TRY NEXT ACTIVE WEEK                         
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R4)                                      
         CLC   DUB(6),WORK+6                                                    
*                                  TRY NEXT ACTIVE WEEK                         
         BNH   CKKFND              ITS COVERED                                  
*                                                                               
CKK9     LA    R7,14               FIND NEXT ACTIVE WEEK                        
         GOTO1 ADDAY,(R1),WORK,EFFST,(R7)                                       
         CLC   EFFST,EFFEND                                                     
         BH    CKK10               NOT IN THIS X'03' ELEMENT                    
         MVC   WORK(6),EFFST                                                    
         B     CKK8                COMPARE NEXT ACTIVE WEEK                     
*                                                                               
CKK10    LA    R3,LTHRDEL(R3)      POINT TO NEXT X'03' ELEMENT                  
         BCT   R2,CKK2                                                          
         LA    R0,MG1ERR           UNCOVERED 6 OR 7 ELEMENT                     
         B     CKKEXIT                                                          
*                                                                               
CKKFND   SR    R0,R0               COVERED ELEMENT                              
*                                                                               
CKKEXIT  XIT1  REGS=(R0)                                                        
         DROP  R3                                                               
***********************************************************************         
* UPDRATE - PERFORM RATE UPDATE                                                 
***********************************************************************         
UPDRATE  NTR1                                                                   
         TM    MODE,DORATE         RATE UPDATE?                                 
         BZ    XIT                 NO - EXIT                                    
         ICM   R4,15,ARATE                                                      
         MVC   RBUYCOS,0(R4)                                                    
         B     XIT                                                              
***********************************************************************         
* UPDDT - PERFORM DAY/TIME CHANGE                                               
***********************************************************************         
UPDDT    NTR1                                                                   
         TM    MODE,DODAY+DOTIME   DAY/TIME UPDATE?                             
         BZ    XIT                 NO - EXIT                                    
         BM    UDT100              ONLY 1                                       
*                                  UPDATE BOTH HERE, JUST REPL BUCKETS          
         GOTO1 VDELELEM,DMCB,(2,RBUYREC)                                        
         LA    R6,BUCKBUFF         NEW 02 BUCKETS                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
UDT050   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,(R6)                                       
         BAS   RE,NEXTEL                                                        
         BE    UDT050                                                           
         MVC   RBUYSTED,BUYSTED                                                 
         B     XIT                                                              
*                                                                               
UDT100   DS    0H                  HANDLE DAY CHANGE ALONE                      
         TM    MODE,DOTIME                                                      
         BO    UDT200                                                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6               R5 POINT TO OLD 02 ELEM                      
         LA    R6,BUCKBUFF                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(2,R5),2(R6)                                                    
         MVC   RBUYSTED,BUYSTED                                                 
         B     XIT                                                              
*                                                                               
UDT200   DS    0H                  HANDLE TIME CHANGE ALONE                     
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6               R5 POINT TO OLD 02 ELEM                      
         LA    R6,BUCKBUFF                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R5),4(R6)                                                    
         B     XIT                                                              
***********************************************************************         
* UPDOCM - PERFORM ORDER COMMENT CHANGE                                         
***********************************************************************         
UPDOCM   NTR1                                                                   
         TM    MODE,DOOCMT                                                      
         BZ    XIT                                                              
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)                                    
         OC    NEWOCMT1,NEWOCMT1                                                
         BZ    UPDOCM10                                                         
         GOTO1 VADDELEM,DMCB,RBUYREC,NEWOCMT1                                   
UPDOCM10 DS    0H                                                               
         OC    NEWOCMT2,NEWOCMT2                                                
         BZ    XIT                                                              
         GOTO1 VADDELEM,DMCB,RBUYREC,NEWOCMT2                                   
         B     XIT                                                              
***********************************************************************         
* UPDCMT - PERFORM COMMENT CHANGE                                               
***********************************************************************         
UPDCMT   NTR1                                                                   
         TM    MODE,DOCMT                                                       
         BZ    XIT                                                              
         GOTO1 VDELELEM,DMCB,(4,RBUYREC)                                        
         OC    NEWCMT1,NEWCMT1                                                  
         BZ    UPDCMT10                                                         
         GOTO1 VADDELEM,DMCB,RBUYREC,NEWCMT1                                    
UPDCMT10 DS    0H                                                               
         OC    NEWCMT2,NEWCMT2                                                  
         BZ    XIT                                                              
         GOTO1 VADDELEM,DMCB,RBUYREC,NEWCMT2                                    
         B     XIT                                                              
***********************************************************************         
* UPDLEN - PERFORM LENGTH CHANGE                                                
***********************************************************************         
UPDLEN   NTR1                                                                   
         TM    MODE,DOLEN                                                       
         BZ    XIT                                                              
         MVC   RBUYDUR,NEWLEN                                                   
         B     XIT                                                              
***********************************************************************         
* UPDFLT - PERFORM FLIGHT NUMBER CHANGE                                         
***********************************************************************         
UPDFLT   NTR1                                                                   
         TM    MODE,DOFLT                                                       
         BZ    XIT                                                              
         MVC   RBUYFLT,NEWFLT                                                   
         B     XIT                                                              
***********************************************************************         
* UPDCAN - PERFORM BUYLINE CANCEL                                               
***********************************************************************         
UPDCAN   NTR1                                                                   
         TM    STAT,CANCEL                                                      
         BZ    XIT                                                              
*                                                                               
         TM    PROFILES+CNTKCANB,CNTKCANA  KEEP CANCELLED BUYS?                 
         BO    UPDCAN30                    YES - SKIP DELETE                    
*                                                                               
         MVC   KEY(27),RBUYKEY                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+27,X'80'        DELETE ACTIVE KEY                            
         GOTO1 VWRITE,DMCB,KEY                                                  
*                                                                               
         MVC   KEY(27),RBUYKEY                                                  
         MVI   KEY,X'9B'                                                        
         MVC   KEY+9(2),REPALPHA                                                
         MVC   KEY+11(4),RCONKADV                                               
         MVC   KEY+15(3),RCONPRD                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   UPDCAN20                                                         
         OI    KEY+27,X'80'        DELETE PASSIVE KEY                           
         GOTO1 VWRITE                                                           
*                                                                               
UPDCAN20 DS    0H                                                               
         OI    RBUYCNTL,X'80'      DELETE RECORD                                
UPDCAN30 DS    0H                                                               
         OI    RBUYFLG2,X'08'      CANCEL INDICATOR                             
         B     XIT                                                              
***********************************************************************         
* VALRATE - VALIDATE NEW RATE FIELDS                                            
***********************************************************************         
VALRATE  NTR1                                                                   
         XC    NEWRATE1(16),NEWRATE1                                            
         LA    R2,MCIRAT1H                                                      
         LA    RF,NEWRATE1                                                      
         STCM  RF,15,ARATE                                                      
         MVC   COMBOCNT,COMBONUM                                                
         B     *+8                                                              
*                                                                               
VRATE010 DS    0H                                                               
         STC   RF,COMBOCNT                                                      
         LA    R3,2                                                             
         CLI   5(R2),0                                                          
         BE    VRATE100                                                         
         OI    MODE,DORATE                                                      
         CLC   =C'NA',8(R2)                                                     
         BNE   VRATE020                                                         
         TM    STAT,COMBO                                                       
         BZ    ERROR                                                            
         ICM   R3,15,ARATE                                                      
         MVC   0(4,R3),=X'FFFFFFFF'                                             
         B     VRATE100                                                         
*                                                                               
VRATE020 DS    0H                                                               
         XC    DMCB+4(3),DMCB+4    CHECK IF VALID CASH VALUE                    
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         ICM   R3,15,ARATE                                                      
         MVC   0(4,R3),DMCB+4      SAVE OFF RATE                                
*                                                                               
VRATE100 DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT RATE FIELD                              
         ICM   RF,15,ARATE                                                      
         LA    RF,4(RF)                                                         
         STCM  RF,15,ARATE         NEXT RATE STORAGE SPOT                       
         ICM   RF,15,ACONNUM                                                    
         LA    RF,4(RF)                                                         
         STCM  RF,15,ACONNUM       NEXT CONTRACT IN COMBO                       
         ZIC   RF,COMBOCNT                                                      
         BCT   RF,VRATE010                                                      
*                                                                               
VRATEX   DS    0H                                                               
         TM    MODE,DORATE                                                      
         BZ    XIT                                                              
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
         B     XIT                                                              
***********************************************************************         
* VALOCMT - VALIDATE NEW ORDER COMMENT FIELD                                    
***********************************************************************         
VALOCMT  NTR1                                                                   
         LA    R2,MCIOCM1H                                                      
         CLI   5(R2),0                                                          
         BNE   VOCMT020                                                         
         CLI   MCIOCM2H+5,0                                                     
         BE    VOCMTX                                                           
*                                                                               
VOCMT020 DS    0H                                                               
         OI    MODE,DOOCMT                                                      
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
*                                                                               
         XC    NEWOCMT1(126),NEWOCMT1                                           
*                                                                               
         LA    R3,2                                                             
         CLI   5(R2),0                                                          
         BE    VOCMT030                                                         
         MVI   NEWOCMT1,X'84'                                                   
         ZIC   RF,5(R2)                                                         
         LA    RF,3(RF)                                                         
         STC   RF,NEWOCMT1+1                                                    
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    *+8                                                              
         MVI   NEWOCMT1+2,X'80'                                                 
         MVC   NEWOCMT1+3(60),8(R2)                                             
*                                                                               
VOCMT030 DS    0H                                                               
         LA    R2,MCIOCM2H                                                      
         CLI   5(R2),0                                                          
         BE    VOCMTX                                                           
         MVI   NEWOCMT2,X'84'                                                   
         ZIC   RF,5(R2)                                                         
         LA    RF,3(RF)                                                         
         STC   RF,NEWOCMT2+1                                                    
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    *+8                                                              
         MVI   NEWOCMT2+2,X'80'                                                 
         MVC   NEWOCMT2+3(60),8(R2)                                             
*                                                                               
VOCMTX   B     XIT                                                              
***********************************************************************         
* VALCMT - VALIDATE NEW COMMENT FIELD                                           
***********************************************************************         
VALCMT   NTR1                                                                   
         LA    R2,MCICMT1H                                                      
         CLI   5(R2),0                                                          
         BNE   VCMT020                                                          
         CLI   MCICMT2H+5,0                                                     
         BE    VCMTX                                                            
*                                                                               
VCMT020  DS    0H                                                               
         OI    MODE,DOCMT                                                       
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
*                                                                               
         XC    NEWCMT1(124),NEWCMT1                                             
*                                                                               
         LA    R3,2                                                             
         CLI   5(R2),0                                                          
         BE    VCMT030                                                          
         CLC   =C'CR=',8(R2)                                                    
         BE    ERROR                                                            
         CLC   =C'MG=',8(R2)                                                    
         BE    ERROR                                                            
         MVI   NEWCMT1,X'04'                                                    
         ZIC   RF,5(R2)                                                         
         LA    RF,2(RF)                                                         
         STC   RF,NEWCMT1+1                                                     
         MVC   NEWCMT1+2(60),8(R2)                                              
*                                                                               
VCMT030  DS    0H                                                               
         LA    R2,MCICMT2H                                                      
         CLI   5(R2),0                                                          
         BE    VCMTX                                                            
         CLC   =C'CR=',8(R2)                                                    
         BE    ERROR                                                            
         CLC   =C'MG=',8(R2)                                                    
         BE    ERROR                                                            
         MVI   NEWCMT2,X'04'                                                    
         ZIC   RF,5(R2)                                                         
         LA    RF,2(RF)                                                         
         STC   RF,NEWCMT2+1                                                     
         MVC   NEWCMT2+2(60),8(R2)                                              
*                                                                               
VCMTX    B     XIT                                                              
***********************************************************************         
* VALLEN - VALIDATE NEW LEN FIELD                                               
***********************************************************************         
VALLEN   NTR1                                                                   
         LA    R2,MCILENH                                                       
         CLI   5(R2),0                                                          
         BE    VLENX                                                            
         OI    MODE,DOLEN                                                       
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
*                                                                               
         LA    R3,LENERR                                                        
         TM    4(R2),X'20'                                                      
         BO    VLENX                                                            
         GOTO1 VPACK               LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0060                                                         
* VALID SECONDS                                                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0020            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0030            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0020 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0040                                                         
LENE0030 EQU   *                                                                
         CH    R0,=H'120'                                                       
         BNH   LENE0040                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
*                                                                               
LENE0040 STH   R0,HALF                                                          
         MVC   NEWLEN,HALF                                                      
         B     VLENX                                                            
* TEST FOR MINUTES                                                              
LENE0060 LA    R4,4                                                             
         LA    R5,MCILEN                                                        
*                                                                               
LENE0080 CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0100                                                         
         CLI   0(R5),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R5),X'F9'                                                      
         BH    ERROR                                                            
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0080                                                      
         B     ERROR                                                            
* PACK MINUTES (MINUTES NOT ALLOWED FOR SPOTPAK XFER)                           
LENE0100 DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0110            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0115            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0110 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0115 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0120 DS    0H                                                               
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   ERROR                                                            
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,MCILEN(0)                                                    
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   NEWLEN,HALF                                                      
         OI    NEWLEN,X'80'       MINUTES IND                                   
*                                                                               
*                                                                               
VLENX    B     XIT                                                              
***********************************************************************         
* VALFLT - VALIDATE NEW FLT FIELD                                               
***********************************************************************         
VALFLT   NTR1                                                                   
         LA    R2,MCIFLTH                                                       
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    VFLTX               YES, DON'T VALIDATE IT                       
         CLI   5(R2),0                                                          
         BE    VFLTX                                                            
         OI    MODE,DOFLT                                                       
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
         CLC   =C'OMIT',8(R2)                                                   
         BNE   VFLT010                                                          
         MVI   NEWFLT,0                                                         
         B     VFLTX                                                            
VFLT010  DS    0H                                                               
         LA    R3,2                                                             
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    ERROR                                                            
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R4,DUB                                                           
         CH    R4,=H'1'                                                         
         BL    ERROR                                                            
         CH    R4,=H'99'                                                        
         BH    ERROR                                                            
         STC   R4,NEWFLT                                                        
VFLTX    B     XIT                                                              
***********************************************************************         
* VALDT - VALIDATE DAY & TIME FIELDS                                            
***********************************************************************         
VALDT    NTR1                                                                   
         LA    R2,MCIDAYSH         DAY INPUT?                                   
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         OI    MODE,DODAY                                                       
*                                                                               
         CLI   MCITIMEH+5,0        TIME INPUT                                   
         BE    *+8                                                              
         OI    MODE,DOTIME                                                      
*                                                                               
         TM    MODE,DODAY+DOTIME    WHAT ARE WE CHANGING?                       
         BZ    VDTX                 NOTHING - OK - GET OUT                      
*                                                                               
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
*                                                                               
         XC    BUCKBUFF(50),BUCKBUFF                                            
*                                                                               
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*              PREPARE TO EDIT STRING OF DAY-TIME FIELDS IN PARALLEL            
         LA    R4,MCIDAYS-1                                                     
         LA    R5,MCIDAYSH                                                      
         LA    R6,MCITIME-1                                                     
         LA    R7,MCITIMEH                                                      
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
DAYTIMED LA    R2,MCIDAYSH                                                      
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         XC    WORK2+2(6),WORK2+2                                               
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAYTIM50                                                         
*              NO DAY LENGTH                                                    
         CLI   MCIDAYSH+5,0        DAY ENTERED?                                 
         BNE   DAYTIM40                                                         
         TM    STAT,ORBIT          IF ORBIT, MUST ENTER DAYS                    
         BZ    DAY101              ELSE SKIP DAY VAILDATION                     
         LA    R3,753                                                           
         B     ERROR                                                            
DAYTIM40 DS    0H                                                               
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
* GET START AND END DAYS FOR ALL 02 ELEMENTS                                    
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,BUYSTED                                                       
*                                                                               
         B     VDTX                EDIT NEXT FIELD                              
DAYTIM50 MVC   DMCB2(4),DMCB+8      DAY FIELD ADDR + LEN                        
*                                                                               
*              EDIT DAY FIELD                                                   
         GOTOX (RFDAYVAL,VREPFACS),DMCB2,,WORK2+3,WORK2+2                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*              GET FIRST START DAY AND LAST END DAY                             
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
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
         BL    DAY100                                                           
* MAKE SURE NO MORE THAN 7 DAYS COVERED                                         
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR               MORE THAN 7 DAYS                             
DAY100   CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
*              EDIT TIME FIELD                                                  
DAY101   DS    0H                                                               
         LA    R2,MCITIMEH                                                      
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   MCITIMEH+5,0                                                     
         BNE   DAY105                                                           
         TM    STAT,ORBIT                                                       
         BZ    DAY105                                                           
         LA    R3,753                                                           
         B     ERROR                                                            
DAY105   DS    0H                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   DAY110                                                           
         TM    STAT,ORBIT          IF NOT ORBIT, PROCESS ANYWAY AS 0            
         BZ    DAY106                                                           
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
DAY106   DS    0H                                                               
         TM    MODE,DODAY          PROCESSING DAY FIELD?                        
         BZ    VDTX                NO - ALL DONE                                
         B     DAY200              PROCESS AS 0                                 
*                                                                               
DAY110   DS    0H                                                               
         ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAY140                                                           
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'VARY'                                                 
         B     DAY200                                                           
*                                                                               
DAY140   EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAY150              OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'NONE'                                                 
         B     DAY200                                                           
*                                                                               
DAY150   MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
         LA    RF,WORK2+4                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY155              6AM - 559AM                                  
         MVI   DMCB+4,X'80'                                                     
         DROP  RF                                                               
*                                                                               
DAY155   DS    0H                                                               
         GOTOX (RFTIMVAL,VREPFACS),DMCB,,         EDIT TIME                     
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAY200              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         B     DAY200              Bypass AM validation                         
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY160              6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
DAY160   DS    0H                                                               
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
*              ADD DAY-TIME ELEMENT TO BUFFER                                   
DAY200   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAY230                                                           
         DROP  RF                                                               
                                                                                
DAY210   DS    0H                                                               
         TM    MODE,DODAY          ONLY VAL CROSS DAY IF CHG DAY                
         BZ    *+8                                                              
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAY230   DS    0H                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         DS    0H                                                               
         GOTO1 VADDELEM,DMCB,BUCKBUFF,WORK2                                     
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     DAYTIMED            EDIT NEXT DAY TIME FIELD COMBO               
*                                                                               
VDTX     B     XIT                                                              
         EJECT                                                                  
*              DELETE ALL DAY-TIME ELEMENTS                                     
         GOTO1 VDELELEM,DMCB,(2,RBUYREC)                                        
*                                                                               
*        SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS                        
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1                                                                   
*                                                                               
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
         XIT1                                                                   
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+16                                                             
         MVI   4(R1),C'*'                                                       
         OI    STAT,ORBIT          SET ORBIT FLAG                               
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   VCROSS20            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         LA    R2,MCIDAYSH                                                      
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
                                                                                
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         LA    R2,MCIDAYSH                                                      
         B     ERROR                                                            
                                                                                
VCROSS20 DS    0H                                                               
         MVC   CROSSD,WORK                                                      
         OI    STAT,UPDCROSS       UPDATE K LATER                               
         B     XIT                                                              
***********************************************************************         
* VALDATE- VALIDATE NEW FLIGHT DATES                                            
***********************************************************************         
VALDATE  NTR1                                                                   
         LA    R2,MCIDATEH                                                      
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BNE   VDAT010                                                          
         TM    MODE,DODAY                                                       
         BZ    VDATX                                                            
         LA    R3,754              MUST CHG DATES WHEN CHG DAYS                 
         B     ERROR                                                            
*                                                                               
VDAT010  DS    0H                                                               
         OI    MODE,DODATE                                                      
*                                                                               
         LA    R3,738              CAN'T CHANGE BUYS WHILE CANCEL               
         TM    STAT,CANCEL                                                      
         BO    ERROR                                                            
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,CONST,0,DUB                       
         L     R3,ACOMFACS                                                      
         GOTOX (RFFLSCAN,VREPFACS),(R1),(R3),CONST,(R2),ENTRIES                 
         CLI   DMCB,0                                                           
         BE    FLTED20                                                          
         ZIC   R3,DMCB                                                          
         B     ERROR                                                            
*                                                                               
FLTED20  MVC   NUMFLTS,DMCB+5                                                   
         GOTO1 GETDAY,(R1),CONST,DMCB+8                                         
         CLC   DMCB+8(3),MYSPACES                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CSTDAY,DMCB         CONTRACT START DAY                           
         GOTO1 (RF),(R1),CONEND,DMCB+8                                          
         CLC   DMCB+8(3),MYSPACES                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CENDAY,DMCB         DAY OF CONTRACT END                          
*                                                                               
FLTED30  DS    0H                                                               
VDATX    B     XIT                                                              
***********************************************************************         
* INLIST - CHECKS IF BUY NUM IN R6 IS ALREADY IN LIST ADDRESSED BY R1           
*            IF NOT IN LIST ADDS BUY TO END CC: NOT EQUAL                       
*            IF ALREADY IN LIST EXITS CC: EQUAL                                 
***********************************************************************         
INLIST NTR1                                                                     
         LR    R4,R1                                                            
INLIST10 DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    INLIST20                                                         
         CLM   R6,1,0(R4)                                                       
         BE    E                                                                
         LA    R4,1(R4)                                                         
         B     INLIST10                                                         
INLIST20 DS    0H                  PUT IT IN LIST                               
         STC   R6,0(R4)                                                         
         B     NE                                                               
***********************************************************************         
* EXPFLT - EXPANDS FLT NUM IN R7 INTO BUYS & PUTS THEM IN BUYLST                
*          OK CC: NOT EQUAL                                                     
*          IF INV FLT OR BUY ALREADY IN LST CC: EQUAL                           
***********************************************************************         
EXPFLT   NTR1                                                                   
         MVI   BYTE,0                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RBUYREC,R4                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,BUYCON                                                  
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         GOTO1 VHIGH                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BNE   E                   NO BUYS - ERROR                              
         B     EXPFLT20                                                         
EXPFLT10 DS    0H                                                               
         LA    R4,KEY                                                           
         GOTO1 VSEQ                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    EXPFLT20                                                         
         CLI   BYTE,X'FF'          HIT 1 BUY FOR FLT?                           
         BE    NE                  YES - NO ERROR                               
         B     E                   NO - ERROR                                   
EXPFLT20 DS    0H                                                               
         CLC   =X'FFFF',RBUYKMLN   SKIP PLAN RECS                               
         BE    EXPFLT10                                                         
         CLC   RBUYKMLN,RBUYKLIN   SKIP MG/CREDITS                              
         BNE   EXPFLT10                                                         
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R4,AIO3                                                          
         CLM   R7,1,RBUYFLT                                                     
         BNE   EXPFLT10                                                         
         MVI   BYTE,X'FF'          WE'VE HIT A BUY FOR FLT                      
         ZICM  R6,RBUYKLIN                                                      
         LA    R1,BUYLST                                                        
         BAS   RE,INLIST                                                        
         BE    E                                                                
         B     EXPFLT10                                                         
         DROP  R4                                                               
***********************************************************************         
* BERROR - BUY VALIDATION ERROR ROUTINE                                         
***********************************************************************         
BERROR   NTR1                                                                   
         EDIT  ERRBUY,(3,FULL),ALIGN=LEFT                                       
         LA    RF,FULL                                                          
         ST    RF,DMCB+12                                                       
         STC   R0,DMCB+12                                                       
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    6(R2),X'40'                                                      
         L     RD,BASERD                                                        
         B     XIT                                                              
***********************************************************************         
* COMMON AREA                                                                   
***********************************************************************         
         LTORG                                                                  
E        DS    0H                                                               
Z        SR    R0,R0               EXIT & CC ROUTINES                           
         B     *+6                                                              
NE       DS    0H                                                               
NZ       LTR   RB,RB                                                            
XIT      XIT1                                                                   
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTF2D                                                       
WORKD    DSECT                                                                  
STAT     DS    X                                                                
INFLT    EQU   X'80'               BUYLINES SPECIFIED BY FLIGHT                 
TVSTA    EQU   X'40'               TV CONTRACT                                  
COMBO    EQU   X'20'               ORDER IS COMBO                               
ORBIT    EQU   X'10'               ORBIT BUYS ON K                              
DID1BUY  EQU   X'08'               PROCESSED AT LEAST 1 BUYLINE                 
CANCEL   EQU   X'04'               CANCEL REQUEST                               
HAVEBUY  EQU   X'02'               INPUT CONTAINS AT LEAST 1 VALID BUY          
UPDCROSS EQU   X'01'               CROSS DAY UPDATED                            
*                                                                               
MODE     DS    X                                                                
DOFLT    EQU   X'80'               CHANGE FLIGHT NUMBER                         
DODAY    EQU   X'40'               CHANGE DAY                                   
DOTIME   EQU   X'20'               CHANGE TIME                                  
DODATE   EQU   X'10'               CHANGE FLIGHT DATE                           
DOLEN    EQU   X'08'               CHANGE BUY LENGTH                            
DORATE   EQU   X'04'               CHANGE BUY RATE                              
DOCMT    EQU   X'02'               CHANGE BUY COMMENT/ORD CMT                   
DOOCMT   EQU   X'01'               CHANGE BUY COMMENT/ORD CMT                   
*                                                                               
BUYCON   DS    CL4                 K NUM 9'S & REVERSED                         
ACONNUM  DS    CL4                 A(CURRENT K NUM IN COMBO LIST)               
CONNUM1  DS    CL4                 K NUM 1                                      
CONNUM2  DS    CL4                       2                                      
CONNUM3  DS    CL4                       3                                      
CONNUM4  DS    CL4                       4                                      
ARATE    DS    CL4                 A(CURRENT RATE)                              
NEWRATE1 DS    CL4                 NEW RATE 1                                   
NEWRATE2 DS    CL4                          2                                   
NEWRATE3 DS    CL4                          3                                   
NEWRATE4 DS    CL4                          4                                   
NEWFLT   DS    X                                                                
NEWLEN   DS    CL2                                                              
ERRBUY   DS    C                                                                
BUYSTED  DS    C                   NEW RBUYSTED                                 
NUMFLTS  DS    C                                                                
MODNUM   DS    X                   UPDATED CONTRACT MOD NUMBER                  
CONST    DS    CL6                 YYMMDD                                       
CONEND   DS    CL6                 YYMMDD                                       
CSTDAY   DS    B                                                                
CENDAY   DS    B                                                                
COMBONUM DS    C                   # CONTRACTS IN COMBO                         
COMBOCNT DS    C                   COMBO # COUNTER FOR LOOPS                    
EFFST    DS    CL6                 USED BY                                      
EFFEND   DS    CL6                 EFFDAT                                       
CONEFFST DS    CL6                                                              
CROSSD   DS    X                   CROSS DAY                                    
ROLLSW   DS    C                   SWITCH SET WHEN END DATE MUST                
*                                  BE ROLLED TO KEEP N WEEKS INTACT             
ENTRIES  DS    CL500                                                            
BUYLST   DS    CL256               BUYLINE LIST                                 
ERRLST   DS    CL256               ERROR BUYLINE LIST                           
NEWCMT1  DS    CL62                                                             
NEWCMT2  DS    CL62                                                             
NEWOCMT1 DS    CL63                                                             
NEWOCMT2 DS    CL63                                                             
BUCKBUFF DS    CL1000              BUYLINE 02 BUCKET BUFFER                     
WORKDX   EQU   *                                                                
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
NFORM    EQU   X'01'               N WEEKS FORMAT                               
DATES    EQU   X'02'               DATE TO DATE FORMAT                          
SONLY    EQU   X'04'               DATE EXPRESSION WAS ONLY 'S'                 
ALT      EQU   X'40'               ALTERNATE WEEKS INDICATOR VALUE              
ENBEFST  EQU   64                                                               
SEQOROUT EQU   132                 DATE SEQUENCE ERROR                          
LTHRDEL  EQU   (RBUYDTWK-RBUYDTEL)+L'RBUYDTWK                                   
*                                                                               
*                                                                               
*DSECT TO COVER FLIGHT DATE ENTRIES                                             
*                                                                               
FLTENTD  DSECT                                                                  
FLTSTART DS    CL6                 YYMMDD - MONDAY OF FIRST WEEK                
FLTEND   DS    CL6                        - SUNDAY OF LAST WEEK                 
FLTIND   DS    B                                                                
*                                  X'80' - EVERY WEEK                           
*                                  X'40' - ALTERNATE WEEKS                      
FLTWKS   DS    B                   NUMBER OF WEEKS                              
FLTCNTL  DS    B                   CONTROL VALUES PASSED BY REFLSCAN            
FLTDAYS  DS    B                   START AND END DAYS                           
*                                  BITS 0-3 START DAY (MONDAY = 1)              
*                                  BITS 4-7 END DAY (SUNDAY = 7)                
FLTUIWKS DS    B                   USER INPUT NUMBER OF WEEKS                   
LFLTENT  EQU   (*-FLTSTART)                                                     
*                                                                               
         CSECT                                                                  
BUCKUP   NTR1  BASE=*,LABEL=*                                                   
         L     R0,VRECUP                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         LA    R5,TWABYTOT         CALCULATE A(WORKSPACE)                       
         AH    R5,=Y(TWABYTRD-TWABYTOT)                                         
         DROP  RF                                                               
*                                                                               
*   NOTE:  THIS IS IN 'TRADE BUCKET TOTAL' AREA IN THE TWA, WHICH               
*        IS NOT USED WITHIN THIS MODULE.                                        
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(0,RBUYREC),                   +        
               (BUCKFLGS,RCONREC),ACOMFACS,VGTBROAD,(R0),(R5)                   
         BE    BUUP0200            EXIT OKAY                                    
         MVC   DUB(4),0(R1)        NO GOOD: STORE ERROR                         
         B     BUUP0220                                                         
BUUP0200 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     BUUP0240                                                         
BUUP0220 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
BUUP0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INIT -INITIALIZE                                                              
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK           PRE VALIDATE HEADER FLIGHT DATES             
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
*                                                                               
         GOTOX (RFVALTCL,VREPFACS),DMCB,(0,RCONREC),VGTBROAD,0,WORK             
         BE    *+16                                                             
         LA    R2,CONDTESH                                                      
         L     R3,0(R1)                                                         
         B     ERROR                                                            
*                                                                               
         MVI   STAT,0              CLEAR FLAGS                                  
         XC    BUYLST,BUYLST                                                    
         XC    ERRLST,ERRLST                                                    
         XC    CONNUM1(16),CONNUM1                                              
         LA    RF,CONNUM1                                                       
         STCM  RF,15,ACONNUM                                                    
*                                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    *+12                                                             
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   *+8                                                              
         OI    STAT,TVSTA          THIS IS A TV CONTRACT                        
*                                                                               
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(3,BUYCON)                 
*                                                                               
* LIST ALL COMBO K NUMS INTO STORAGE                                            
         GOTOX (RFCMBNUM,VREPFACS),DMCB,RCONREC                                 
         MVC   COMBONUM,0(R1)                                                   
         CLI   0(R1),1                                                          
         BNE   INIT010                                                          
         MVC   CONNUM1,RCONKCON                                                 
         B     INIT030                                                          
INIT010  DS    0H                                                               
         OI    STAT,COMBO                                                       
         ZIC   R3,0(R1)                                                         
         ZICM  R6,1(R1),3                                                       
         LA    R6,7(R6)                                                         
         LA    R5,CONNUM1                                                       
INIT020  DS    0H                                                               
         MVC   0(4,R5),0(R6)                                                    
         LA    R6,9(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,INIT020                                                       
*                                                                               
INIT030  DS    0H                                                               
         CLC   =C'CAN',CONBNUM                                                  
         BNE   *+8                                                              
         OI    STAT,CANCEL                                                      
*                                                                               
         CLC   =C'NEX',CONBNUM     CLEAR DEBRIS FROM NUM FIELD                  
         BNE   INIT0040                                                         
         MVC   CONBNUM(4),MYSPACES                                              
         LA    RF,CONBNUMH                                                      
         OI    6(RF),X'80'         TRANSMIT CLEARED FIELD NEXT TIME             
INIT0040 EQU   *                                                                
*                                                                               
         B     XIT                                                              
***********************************************************************         
* CKBUYS - CHECK BUYLINES BEFORE UPDATING                                       
***********************************************************************         
CKBUYS   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RBUYREC,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,BUYCON                                                  
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         LA    R4,BUYLST                                                        
         B     *+8                                                              
CKB020   DS    0H                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BE    CKBX                                                             
         CLI   0(R4),X'FF'                                                      
         BE    CKB020                                                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   KEY(25),WORK                                                     
         MVC   RBUYKMLN,0(R4)                                                   
         MVC   RBUYKLIN,0(R4)                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         MVC   ERRBUY,RBUYKLIN                                                  
*                                                                               
         BAS   RE,VBCAN                                                         
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   CKB030                                                           
         BAS   RE,NEXTEL                                                        
         BNE   CKB030                                                           
         OI    STAT,ORBIT          BUY HAS ORBIT                                
*                                                                               
CKB030   DS    0H                                                               
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    CKB035              NO -OK                                       
         LA    R3,879              ERROR - MCI AGAINST SPORTS BUYS              
         MVC   ERRBUY,0(R4)                                                     
         LA    R2,MCIOLDBH                                                      
         BAS   RE,BERROR                                                        
*                                                                               
CKB035   DS    0H                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BNE   CKB040              NO -OK                                       
         LA    R3,2                                                             
         LA    R2,MCIOLDBH                                                      
         BAS   RE,ERROR                                                         
*                                                                               
*** VALIDATE BUY RECORD FURTHER HERE ***                                        
*                                                                               
CKB040   DS    0H                                                               
         GOTO1 VSEQ                                                             
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CKB020                                                           
         TM    MODE,DODAY+DODATE                                                
         BZ    CKB020                                                           
         CLC   RBUYKMLN,0(R4)      DOES BUY REFER TO PREV BUY?                  
         BNE   CKB020              NO - OK                                      
         LA    R3,746                                                           
         MVC   ERRBUY,0(R4)                                                     
         LA    R2,MCIOLDBH                                                      
         BAS   RE,BERROR                                                        
         DROP  R6                                                               
*                                                                               
CKBX     B     XIT                                                              
***********************************************************************         
* VBCAN - VALIDATE CANCEL ACTION FOR EACH BUY                                   
***********************************************************************         
VBCAN    NTR1                                                                   
         TM    STAT,CANCEL                                                      
         BZ    VBCX                                                             
*                                                                               
         LA    R3,748                                                           
         LA    R5,RBUYELEM                                                      
         SR    R2,R2               WON'T ALLOW CANCEL IF BUY HAS                
CAN50    CLI   0(R5),0                                                          
         BE    CAN60                                                            
         CLI   0(R5),5             MAKE GOOD                                    
         BE    CERROR                                                           
         CLI   0(R5),6             OR MISSED ELEMENT                            
         BE    CERROR                                                           
         CLI   0(R5),7             OR MISSED CREDIT ISSUED                      
         BE    CERROR                                                           
         CLI   0(R5),X'56'         OR MG SPLITOUT BUY MISSED ELEMENT            
         BE    CERROR                                                           
         CLI   0(R5),X'66'         OR MG MISSED ELEMENT                         
         BE    CERROR                                                           
         IC    R2,1(R5)            NEXT ELEMENT                                 
         AR    R5,R2                                                            
         B     CAN50                                                            
*                                                                               
CERROR   LA    R2,MCIOLDBH                                                      
         MVC   ERRBUY,RBUYKMLN                                                  
         BAS   RE,BERROR                                                        
*                                                                               
CAN60    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CAN80                                                            
         USING RCONSEND,R6                                                      
*                                                                               
         CLI   RBUYCHGI,C'A'       OK TO CANCEL IF MOD CODE <> 'A'              
         BNE   CAN80                                                            
         CLC   RBUYVER,RCONSRV     IF MOD CODE='A', CHECK BUY VERSION           
         BNE   CAN80               AGAINST K VERSION                            
*                                  OK TO CANCEL SINCE UNEQUAL VERSIONS          
*                                  IMPLIES K WAS SENT                           
         CLI   TWAACCS,C'$'        STATION                                      
         BE    CAN70                                                            
         TM    RCONSENF,X'20'      SENT BY REP                                  
         BO    CAN80                                                            
         LA    R3,750              BUYLINE HAS BEEN SENT                        
         B     CERROR                                                           
*                                                                               
CAN70    DS    0H                                                               
         TM    RCONSENF,X'10'                                                   
         BO    CAN80                                                            
         LA    R3,750              BUYLINE HAS BEEN SENT                        
         B     CERROR                                                           
         DROP  R6                                                               
*                                                                               
CAN80    DS    0H                                                               
*&&DO                                                                           
*  *** SKIP CHECK FOR CANCEL ACTION ***                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    CAN82               NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    CAN84               YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
CAN82    EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   CAN90                                                            
CAN84    EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   CAN90                                                            
         USING RBUYSPEL,R6                                                      
         CLI   RBUYSPL#,0          HAS BEEN XFERRED?                            
         BE    CAN90               NO, OKAY TO DEL                              
         DROP  R6                                                               
*                                                                               
         LA    R3,269              CAN'T DEL BUY XFER'D TO SPOT                 
         B     CERROR                                                           
*&&                                                                             
*                                                                               
CAN90    DS    0H                                                               
VBCX     B     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* VALOFLT - VALIDATE OLD FLIGHT FIELD                                           
***********************************************************************         
VALOFLT  NTR1  BASE=*,LABEL=*                                                   
         TM    STAT,INFLT          FLIGHT SPECIFIED?                            
         BZ    VOFLTX              NO                                           
*                                                                               
         LA    R2,MCIOLDFH                                                      
         LA    R3,2                                                             
         GOTO1 SCANNER,DMCB,(R2),AIO4,C',=,-'                                   
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         ZIC   R4,4(R1)            # OF SCANNER LINES                           
         L     R5,AIO4             SCANNER OUTPUT                               
*                                                                               
VOFLT010 DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    ERROR                                                            
         TM    2(R5),X'80'         1ST FIELD NUMERIC                            
         BZ    ERROR                                                            
         ICM   R7,15,4(R5)         BINARY 1ST FLD                               
         C     R7,=F'255'                                                       
         BH    ERROR                                                            
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
         CLI   1(R5),0             2ND FIELD                                    
         BNE   VOFLT020            YES                                          
         BAS   RE,EXPFLT           NO - SINGLE FLT, EXPAND FLIGHT               
         BE    ERROR                                                            
         B     VOFLT040                                                         
*                                                                               
VOFLT020 DS    0H                                                               
         TM    3(R5),X'80'         2ND FIELD NUMERIC                            
         BZ    ERROR                                                            
         ICM   RF,15,8(R5)                                                      
         C     RF,=F'255'                                                       
         BH    ERROR                                                            
         CR    RF,R7                                                            
         BNH   ERROR                                                            
VOFLT030 DS    0H                                                               
         BAS   RE,EXPFLT           EXPAND FLIGHT                                
         BE    ERROR                                                            
         CR    R7,RF                                                            
         BNL   VOFLT040                                                         
         LA    R7,1(R7)                                                         
         B     VOFLT030                                                         
*                                                                               
VOFLT040 DS    0H                                                               
         LA    R5,32(R5)                                                        
         BCT   R4,VOFLT010         NEXT SCANNER LINE                            
*                                                                               
VOFLTX   DS    0H                                                               
         B     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* VALBUY - VALIDATE BUYLINE FIELD                                               
***********************************************************************         
VALBUY   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MCIOLDBH                                                      
         CLI   5(R2),0             BUYLINE # ENTERED?                           
         BNE   VBUY010             YES                                          
*                                                                               
         LA    R3,1                                                             
         TM    STAT,TVSTA          TV CONTRACT?                                 
         BO    ERROR               REQURED FIELD                                
*                                                                               
         CLI   MCIOLDFH+5,0        OLD FLT NUM?                                 
         BE    ERROR                                                            
         OI    STAT,INFLT          INPUT IS FLT NUM                             
         B     XIT                                                              
*                                                                               
VBUY010  DS    0H                                                               
         TM    STAT,TVSTA                                                       
         BO    VBUY020                                                          
*                                                                               
         LA    R3,736                                                           
         CLI   MCIOLDFH+5,0                                                     
         BNE   ERROR               CAN'T ENTER NUM & FLT                        
*                                                                               
VBUY020  DS    0H                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VBUY050                                                          
         LA    R3,2                                                             
         CLI   5(R2),3                                                          
         BNE   ERROR                                                            
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RBUYKEY,R5                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,BUYCON                                                  
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         GOTO1 VHIGH                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    VBUY040                                                          
         LA    R3,747                                                           
         B     ERROR                                                            
VBUY030  DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VBUYX                                                            
VBUY040  DS    0H                                                               
         CLC   =X'FFFF',RBUYKMLN                                                
         BE    VBUY030                                                          
         CLC   =X'FFFFFF',RBUYKPLN                                              
         BNE   VBUY030                                                          
         ZIC   R6,RBUYKLIN                                                      
         LA    R1,BUYLST                                                        
         CLC   RBUYKMLN,RBUYKLIN                                                
         BE    *+8                                                              
         LA    R1,ERRLST                                                        
         BAS   RE,INLIST                                                        
         BNE   VBUY030                                                          
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
VBUY050  DS    0H                                                               
         LA    R3,2                                                             
         GOTO1 SCANNER,DMCB,(R2),AIO4,C',=,-'                                   
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         ZIC   R4,4(R1)            # OF SCANNER LINES                           
         L     R5,AIO4             SCANNER OUTPUT                               
*                                                                               
VBUY060  DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    ERROR                                                            
         TM    2(R5),X'80'         1ST FIELD NUMERIC                            
         BZ    ERROR                                                            
         ICM   R6,15,4(R5)         BINARY 1ST FLD                               
         C     R6,=F'255'                                                       
         BH    ERROR                                                            
         LTR   R6,R6                                                            
         BZ    ERROR                                                            
         CLI   1(R5),0             2ND FIELD                                    
         BNE   VBUY070             YES                                          
         LA    R1,BUYLST                                                        
         BAS   RE,INLIST           NO - SINGLE BUY                              
         BE    ERROR                                                            
         B     VBUY090                                                          
*                                                                               
VBUY070  DS    0H                                                               
         TM    3(R5),X'80'         2ND FIELD NUMERIC                            
         BZ    ERROR                                                            
         ICM   RF,15,8(R5)                                                      
         C     RF,=F'255'                                                       
         BH    ERROR                                                            
         CR    RF,R6                                                            
         BNH   ERROR                                                            
VBUY080  DS    0H                                                               
         LA    R1,BUYLST                                                        
         BAS   RE,INLIST                                                        
         BE    ERROR                                                            
         CR    R6,RF                                                            
         BNL   VBUY090                                                          
         LA    R6,1(R6)                                                         
         B     VBUY080                                                          
*                                                                               
VBUY090  DS    0H                                                               
         LA    R5,32(R5)                                                        
         BCT   R4,VBUY060          NEXT SCANNER LINE                            
*                                                                               
         LA    R4,BUYLST                                                        
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RBUYKEY,R5                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,BUYCON                                                  
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         MVC   WORK(25),KEY                                                     
         B     *+10                                                             
VBUY100  DS    0H                                                               
         MVC   KEY(25),WORK                                                     
         CLI   0(R4),0                                                          
         BE    VBUYX                                                            
         MVC   RBUYKMLN,0(R4)                                                   
         MVC   RBUYKLIN,0(R4)                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   *+12                                                             
         OI    STAT,HAVEBUY                                                     
         B     VBUY120                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),WORK                                                     
         GOTO1 VHIGH                                                            
         B     VBUY106                                                          
VBUY105  GOTO1 VSEQ                                                             
VBUY106  CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         CLC   RBUYKLIN,0(R4)                                                   
         BNE   VBUY105                                                          
         ZIC   R6,RBUYKLIN                                                      
         MVI   0(R4),X'FF'                                                      
         LA    R1,ERRLST                                                        
         BAS   RE,INLIST                                                        
         BNE   VBUY120                                                          
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
VBUY120  DS    0H                                                               
         LA    R4,1(R4)                                                         
         B     VBUY100                                                          
         DROP  R5                                                               
*                                                                               
VBUYX    DS    0H                                                               
         LA    R4,BUYLST                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         CLI   0(R4),0                                                          
         BNE   XIT                                                              
         LA    R3,747                                                           
         B     ERROR                                                            
         LTORG                                                                  
*                                                                               
***********************************************************************         
* GENECKEY - WRITE CONTRACT EC KEY                                              
***********************************************************************         
GENECKEY NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'EC'           ESTABLISH SAR KEY                            
         LR    R3,RA                                                            
         AH    R3,=Y(TWAWORKQ)                                                  
*                                                                               
         USING TWAWORK,R3                                                       
*                                                                               
         MVC   KEY+23(4),TWACNUM                                                
         MVC   KEY+21(2),REPALPHA                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    GENEXIT             ALREADY THERE - DON'T READD                  
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
GENEXIT  EQU   *                                                                
         XMOD1                                                                  
*                                                                               
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
* GENMSG - GENERATE ERR/INF MSG FOR COMPLETION EXIT                             
***********************************************************************         
GENMSG   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ERRLST           NO ERROR CASE                                
         CLI   0(R4),0                                                          
         BNE   GMSG050                                                          
         LA    R3,46                                                            
         GOTO1 GETTXT,DMCB,(R3),0,(C'I',DMCB),0,0,0                             
         B     XIT                                                              
*                                                                               
GMSG050  DS    0H                  NONE PROCESSED CASE                          
         TM    STAT,DID1BUY                                                     
         BO    GMSG100                                                          
         LA    R3,768                                                           
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),0,0,0                             
         B     XIT                                                              
*                                                                               
GMSG100  DS    0H                                                               
         LA    R4,ERRLST                                                        
         XC    WORK3,WORK3                                                      
         LA    R5,WORK3                                                         
GMSG120  DS    0H                                                               
         EDIT  (1,0(R4)),(4,0(R5)),ALIGN=LEFT                                   
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   GMSG120                                                          
*                                                                               
         BCTR  R5,0                                                             
         MVI   0(R5),0                                                          
         LA    RF,WORK3                                                         
         SR    R5,RF                                                            
         CH    R5,=H'17'                                                        
         BNH   GMSG140                                                          
         MVC   0(4,RF),=C'buys'                                                 
         LA    R5,4                                                             
GMSG140  DS    0H                                                               
         ST    RF,DMCB+12                                                       
         STC   R5,DMCB+12                                                       
         LA    R3,749                                                           
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         LA    R2,CONBACTH                                                      
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
*                                                                               
***********************************************************************         
* SUB-ROUTINE FOR CONSTRUCTING EFFECTIVE DATE ELEMENTS FROM                     
* ENTRIES GENERATED BY REFLSCAN                                                 
***********************************************************************         
EFFDAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R4,TEMP                                                          
         ZIC   R2,NUMFLTS                                                       
         LA    R6,ENTRIES          R6 POINTS TO ENTRIES                         
         USING FLTENTD,R6                                                       
         XC    WORK2,WORK2                                                      
         LA    R7,WORK2            R7 POINTS TO ELEMENTS                        
         USING RBUYDTEL,R7                                                      
*                                                                               
EFF10    SR    R4,R4                                                            
         SR    R5,R5                                                            
         LA    RE,RBUYREC                                                       
         IC    R4,RBUYSTED-RBUYKEY(RE)                                          
         SRDL  R4,4                R4 HAS START DAY                             
         SRL   R5,28               R5 HAS END DAY                               
*                                                                               
EFF30    MVI   RBUYDTEL,X'03'      ELEMENT CODE                                 
         MVI   RBUYDTLN,LTHRDEL    ELEMENT LENGTH                               
         MVC   RBUYDTWK,FLTWKS     NUMBER OF WEEKS                              
         LR    R3,R4                                                            
         CLC   FLTSTART,CONST      DOES ENTRY START IN FIRST WEEK OF            
         BH    EFF60               CONTRACT-NO                                  
         CLM   R3,1,CSTDAY         DOES BUY ST DAY PRECEDE CONST DAY            
         BNL   EFF60               NO                                           
*                                                                               
EFF40    LA    R3,7(R3)            PUSH BACK EFFECTIVE START 1 WEEK             
         TM    FLTCNTL,SONLY       INPUT OF JUST 'S' AND BUY START              
         BO    EFF50               BEFORE CON ST IS AN ERROR                    
         TM    FLTCNTL,NFORM                                                    
         BNO   *+12                FOR N WEEKS FORMAT, SET SWITCH TO            
         MVI   ROLLSW,YES          ROLL BACK END DATE SO SCHEDULE WILL          
         B     EFF60               SPAN EXACTLY N WEEKS                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                                                              
         DC    H'0'                FOR DATES FORMAT, DECREASE WEEKS             
*        ZIC   R1,RBUYDTWK         SPANNED BY ONE TO REFLECT ROLLED             
*        BCTR  R1,0                BACK START DATE                              
*        STC   R1,RBUYDTWK                                                      
         B     EFF60                                                            
*                                                                               
EFF50    LA    R0,SDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
EFF60    DS    0H                                                               
         BCTR  R3,0                SUBTRACT ONE FOR ADDING TO MONDAY            
         LTR   R3,R3               IS BUY START DAY MONDAY                      
         BZ    EFF70               YES                                          
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R3)                                   
         B     EFF80                                                            
*                                                                               
EFF70    MVC   EFFST,FLTSTART      ALREADY MONDAY                               
*                                                                               
EFF80    DS    0H                                                               
         BAS   RE,GETEFFST                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*        GOTO1 ADDAY,DMCB,EFFST,EFFST,F'7'                                      
*                                                                               
EFF85    DS    0H                                                               
         LR    R3,R5                                                            
         CLC   FLTEND,CONEND       DOES ENTRY END IN LAST WEEK OF CON           
         BL    EFF110              NO                                           
         CLM   R3,1,CENDAY         BUY END DAY VS. CONTRACT END DAY             
         BH    EFF90               ITS HIGHER-ROLL BACK EFFECTIVE DATES         
         B     EFF110                                                           
*                                                                               
EFF90    SH    R3,=H'14'           SUBTRACT TO FIND NUMBER FOR ADDAY            
*        ZIC   R0,RBUYDTWK         DECREMENT NUMBER OF WEEKS                    
*        SH    R0,=H'1'                                                         
*        BZ    EFF100                                                           
*        STC   R0,RBUYDTWK                                                      
         TM    FLTCNTL,NFORM       RECOGNIZE END DATE ERROR IF N WEEKS          
         BZ    EFF120              CANNOT FIT WITHIN CONTRACT                   
*                                                                               
EFF100   LA    R0,EDTERR           ENT IS LAST WEEK ONLY AND HAD TO             
         B     EFFEXIT             BE ROLLED BACK                               
*                                                                               
EFF110   SH    R3,=H'7'                                                         
         BM    EFF120              DAY IS NOT SUNDAY                            
         MVC   EFFEND,FLTEND                                                    
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
EFF115   DS    0H                  SKIP ADDAY FOR BUY END DAY OF                
         CLI   ROLLSW,YES          SUNDAY. FOR BUY END DAY OF SUNDAY,           
         BE    EFF130              FIRST CHECK FOR ROLL SWITCH AND              
         B     EFF140              BRANCH OR SKIP ADDAY IF ROLL IS OFF.         
*                                                                               
EFF120   GOTO1 ADDAY,DMCB,FLTEND,EFFEND,(R3)                                    
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
EFF125   DS    0H                                                               
         CLI   ROLLSW,YES          ONLY ENTER THIS ROUTINE WHEN                 
         BNE   EFF140              SWITCH IS SET TO ROLL BACK END DATE          
*                                                                               
EFF130   MVI   ROLLSW,NO                                                        
         MVC   DMCB+8(4),=F'7'     ROLL BACK END DATE 7 DAYS UNLESS             
         CR    R4,R5               END DAY IS GT START DAY                      
         BNH   *+10                                                             
         MVC   DMCB+8(4),=F'14'                                                 
*        GOTO1 ADDAY,DMCB,EFFEND,EFFEND                                         
         CLC   EFFEND,CONEND                                                    
         BNH   *+12                                                             
         LA    R0,EDTERR           RUN PAST CONTRACT END                        
         B     EFFEXIT                                                          
         CH    R2,=H'1'            ANY MORE ENTRIES                             
         BE    EFF180              NO                                           
         LA    RF,LFLTENT(R6)      YES-POINT TO NEXT ENTRY AND CHECK            
         CLC   EFFEND,0(RF)        END DATE VS. MONDAY OF NEXT START            
         BL    EFF180              WEEK.                                        
         LA    R0,SEQOROUT         SEQUENCE ERROR-LAST SET OF EFFECTIVE         
         B     EFFEXIT             DATES WILL OVERLAP NEXT FLIGHT ENTRY         
*                                                                               
EFF140   CR    R4,R5               ADJUSTMENT LOGIC ENTERED ONLY                
         BNH   EFF160              WHEN START DAY GREATER THAN END DAY          
         TM    FLTCNTL,NFORM                                                    
         BO    EFF150              N WEEKS INPUT FORMAT                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                 DATE-DATE INPUT FORMAT                       
         DC    H'0'                                                             
*        ZIC   R1,RBUYDTWK         SUBTRACT 1 TO ADJUST FOR                     
*        BCTR  R1,0                BUY WEEKS SPANNING                           
*        STC   R1,RBUYDTWK         2 WEEKS OF CONTRACT.                         
         B     EFF160                                                           
*                                                                               
EFF150   MVC   DUB(6),EFFEND       PUSH BACK END DATE 7DAYS FOR N WKS           
*        GOTO1 ADDAY,DMCB,DUB,EFFEND,7                                          
         CLC   EFFEND,CONEND                                                    
         BNH   EFF160                                                           
         LA    R0,EDTERR           RUNS PAST CONTRACT END                       
         B     EFFEXIT                                                          
*                                                                               
EFF160   CLC   EFFST,EFFEND        CHECK SEQUENCE OF EFFECTIVE                  
         BL    EFF180              START/END                                    
         BE    EFF170                                                           
         LA    R0,ENBEFST          SEQUENCE ERROR                               
         B     EFFEXIT                                                          
*                                                                               
EFF170   CR    R4,R5               START/END MUST BE                            
         BE    EFF180              IDENTICAL                                    
         LA    R0,2                                                             
         B     EFFEXIT                                                          
*                                                                               
EFF180   GOTO1 DATCON,DMCB,(0,EFFST),(3,RBUYDTST)                               
         GOTO1 (RF),(R1),(0,EFFEND),(3,RBUYDTED)                                
         MVC   RBUYDTIN,FLTIND                                                  
EFF190   LA    RE,RBUYREC                                                       
         MVC   RBUYDTNW,RBUYNW-RBUYKEY(RE)                                      
         TM    RBUYDTIN,ALT        FOR ALTERNATE WEEKS ONLY                     
         BZ    EFF200                                                           
         ZIC   R1,RBUYDTWK         TAKE TOTAL WEEKS                             
         LA    R1,1(R1)            AND AFTER ADDING 1 TO STOP                   
         SRL   R1,1                ROUNDING DOWN, DIVIDE BY TWO                 
         STC   R1,RBUYDTWK         TO FIND NUMBER OF ACTIVE WEEKS               
*                                                                               
EFF200   LA    R7,LTHRDEL(R7)                                                   
         LA    R6,LFLTENT(R6)                                                   
         BCT   R2,EFF30                                                         
         XR    R0,R0               SUCCESSFUL PROCESSING                        
*                                                                               
EFFEXIT  DS    0H                                                               
         XIT1  REGS=(R0)                                                        
         EJECT                                                                  
***********************************************************************         
* R4 START DAY                                                                  
* R5 END DAY                                                                    
* R6 POINTS TO ENTRIES WITH FLIGHT DATE INFO                                    
*                                                                               
* ROUTINE TO CALCULATE ACTUAL EFFECTIVE START AND END DATES                     
*                                                                               
* NOTE: RECNT18 ORIGINALLY SUPPORTS ONLY STANDARD WEEK ROTATIONS.               
*       IT CALLS REFLSCAN TO BUILD A TABLE WITH MON-SUN DATES.                  
*       THESE ROUTINES MASSAGES THE TABLE AND RECALCULATE NUMBER OF             
*       WEEKS, SO OUT-OF-WEEK-ROTATORS ARE SUPPORTED                            
***********************************************************************         
GETEFFST NTR1                                                                   
         CLC   CSTDAY,CENDAY                                                    
         BH    GETST20                                                          
*                                                                               
* STANDARD WEEK ONLY. START FROM CONTRACT START DATE OR LATER                   
*                                                                               
         CLC   FLTSTART,CONST      SAME WEEK AS CONTRACT START DATE?            
         BL    GETST10                                                          
         MVC   EFFST,FLTSTART      NO                                           
         LA    R2,1                START WITH MONDAY                            
         LR    R3,R4                                                            
         BCTR  R3,0                                                             
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R3)                                   
         MVC   CONEFFST,EFFST                                                   
         B     GETST80                                                          
*                                                                               
GETST10  DS    0H                  SAME WEEK AS CONTRACT START DATE             
         MVC   EFFST,CONST         SO, USE CONTRACT START DATE                  
         ZIC   R2,CSTDAY           AND CONTRACT START DAY, TOO                  
         MVC   CONEFFST,EFFST                                                   
*                                                                               
         CR    R2,R4                                                            
         BE    GETST80                                                          
         LR    R3,R4                                                            
         CR    R3,R2                                                            
         BE    GETST80                                                          
         BH    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R3)                                      
         B     GETST80                                                          
*                                                                               
* FOR OUT-OF-WEEK-ROTATORS                                                      
*                                                                               
GETST20 DS     0H                                                               
* CANNOT CROSS AN OUT-OF-WEEK WEEK                                              
         ZIC   R3,CSTDAY                                                        
         CR    R4,R3                                                            
         BNL   GETST25                                                          
         CR    R5,R3                                                            
         BL    GETST25                                                          
         LA    R0,613                                                           
         B     EFFEXIT                                                          
****                                                                            
GETST25 DS     0H                                                               
         ZIC   R2,FLTDAYS          START/END DAY                                
         SRL   R2,4                REMOVE END DAY                               
         LTR   R2,R2               DID USER SPECIFIED S FOR START DAY?          
         BNZ   GETST30                                                          
         ZIC   R2,CSTDAY           YES, USE CONTRACT START DAY                  
*                                                                               
GETST30  DS    0H                                                               
         LR    R3,R2                                                            
         BCTR  R2,0                                                             
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R2)                                   
*                                                                               
* FIRST, CALCULATE THE EFFECTIVE START DATE BASED ON THE ROTATION OF            
* THE CONTRACT FLIGHT DATES                                                     
*                                                                               
         ZIC   R2,CSTDAY                                                        
         CR    R3,R2                                                            
         BE    GETST40                                                          
         BH    *+8                 BACK UP TO EFFECTIVE START DAY               
         SH    R2,=H'7'                                                         
         SR    R2,R3                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R2)                                      
*                                                                               
* NEXT, FIND THE ACTUAL EFFECTIVE START DAY BASED ON THE BUY'S START            
* DAY                                                                           
*                                                                               
GETST40  DS    0H                                                               
         MVC   CONEFFST,EFFST      SAVE OFF FOR END DATE LATER                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFST,DMCB+8                                         
         ZIC   R2,DMCB                                                          
         LR    R3,R4                                                            
*                                                                               
         CR    R3,R2                                                            
         BE    GETST80                                                          
         BH    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R3)                                      
*                                                                               
GETST80  DS    0H                  START DATE ON/AFTER CONTRACT START?          
         CLC   EFFST,CONST                                                      
         BNL   GETST90                                                          
         LA    R0,SDTERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETST90  DS    0H                  START DATE AND DAY EQUAL?                    
         GOTO1 GETDAY,DMCB,EFFST,DMCB+8                                         
         ZIC   R3,DMCB                                                          
         CR    R3,R4                                                            
         BE    GETEX               YES, EXIT                                    
         LA    R0,SDYERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETEX    DS    0H                                                               
         SR    R0,R0                                                            
         B     EFFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* R4 START DAY                                                                  
* R5 END DAY                                                                    
*                                                                               
* ROUTINE TO CALULATE END DATES                                                 
*                                                                               
***********************************************************************         
GETEFFED NTR1                                                                   
         CLC   CSTDAY,CENDAY                                                    
         BH    GETED40                                                          
***********************************************************************         
* FOR STANDARD IN-WEEK ROTATORS                                                 
***********************************************************************         
         MVC   EFFEND,FLTEND                                                    
         LA    R2,7                                                             
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED30                                                          
         SR    R3,R2                                                            
         CR    R4,R5               USER SPECIFIED OOW BUY                       
         BH    GETED10                                                          
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED20                                                          
*                                                                               
GETED10  DS    0H                  BUY IS OOW                                   
         AH    R3,=H'7'                                                         
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BNE   GETED30                                                          
*                                                                               
GETED20  DS    0H                  FIND BEST FIT                                
         CLC   EFFEND,FLTEND       WENT PAST FLIGHT END DATE                    
         BNH   GETED30             BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED20                                                          
*                                                                               
GETED30  DS    0H                  FIND BEST FIT                                
         CLC   EFFEND,CONEND       WENT PAST CONTRACT END DATE                  
         BNH   GETED110            BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED30                                                          
         EJECT                                                                  
***********************************************************************         
* FOR OOWR                                                                      
***********************************************************************         
GETED40  DS    0H                                                               
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BNE   GETED50                                                          
         ZIC   R2,FLTDAYS          DID USER SPECIFY END DATE?                   
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         LTR   R2,R2                                                            
         BNZ   GETED80                                                          
         CLI   FLTWKS,1            SINGLE DATE?                                 
         BNE   GETED60             NO MUST BE -E                                
         ZIC   R2,FLTWKS           YES, FAKE -1W                                
         B     GETED55                                                          
*                                                                               
***********************************************************************         
* CASE: -NW (IE -3W)                                                            
***********************************************************************         
*                                                                               
GETED50  DS    0H                                                               
         ZIC   R2,FLTUIWKS         YES, CALCULATE END DATE                      
GETED55  MH    R2,=H'7'                                                         
         BCTR  R2,0                                                             
         GOTO1 ADDAY,DMCB,EFFST,EFFEND,(R2)                                     
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED110                                                         
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED110                                                         
*                                                                               
***********************************************************************         
* CASE: -E                                                                      
***********************************************************************         
GETED60  DS    0H                                                               
         GOTO1 PERVERT,DMCB,EFFST,CONEND                                        
         ZICM  R2,DMCB+12,2        NUMBER OF WEEKS IN DATES                     
         OC    DMCB+10(2),DMCB+10                                               
         BZ    *+8                 IF REMAINDER, COUNT AS ANOTHER WEEK          
         LA    R2,1(R2)                                                         
         MH    R2,=H'7'            GET THE NUMBER OF DAYS TO END DATE           
         BCTR  R2,0                MAKE START DAY RELATIVE                      
         GOTO1 ADDAY,DMCB,CONEFFST,EFFEND,(R2)                                  
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED70                                                          
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* IF EFFECTIVE END DATE IS PAST CONTRACT END DATE, BACK UP UNTIL FIT            
*                                                                               
GETED70  DS    0H                                                               
         CLC   EFFEND,CONEND       WENT PAST CONTRACT END DATE                  
         BNH   GETED110            BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED70                                                          
*                                                                               
***********************************************************************         
* CASE: -MMMDD (IE -MAY21)                                                      
***********************************************************************         
*                                                                               
GETED80  DS    0H                                                               
         MVC   EFFEND,FLTEND                                                    
         ZIC   R2,FLTDAYS                                                       
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         LA    R3,7                                                             
         SR    R2,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R2)                                    
*                                                                               
* FIND THE END DATE AS SPECIFIED BY THE CONTRACT END DAY                        
*                                                                               
         ZIC   R2,FLTDAYS                                                       
         SLL   R2,28                                                            
         SRL   R2,28                                                            
*                                                                               
         ZIC   R3,CSTDAY           ONE COMPLETE ROTATION MEANS                  
         BCTR  R3,0                THE CONTRACT END DAY IS ALWAYS               
         LTR   R3,R3               ONE LESS THAN CONTRACT START DAY             
         BNZ   *+8                 REGARLESS OF THE ACTUAL CONTRACT             
         LA    R3,7                END DATE                                     
*                                                                               
         CR    R2,R3                                                            
         BE    GETED90                                                          
         BL    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
GETED90  DS    0H                                                               
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED110                                                         
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* VALID RANGE CHECKS                                                            
*                                                                               
GETED110 DS    0H                                                               
         CLC   EFFEND,CONEND                                                    
         BNH   GETED120                                                         
         LA    R0,EDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
GETED120 DS    0H                                                               
         CLC   EFFEND,EFFST                                                     
         BNL   GETED130                                                         
         LA    R0,ENBEFST                                                       
         B     EFFEXIT                                                          
*                                                                               
GETED130 DS    0H                  END DATE AND DAY EQUAL ?                     
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R3,DMCB                                                          
         CR    R3,R5                                                            
         BE    GETED140            YES, EXIT                                    
         LA    R0,EDYERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETED140 DS    0H                                                               
         BAS   RE,ADJUSTWK                                                      
*                                                                               
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BE    GETEDX                                                           
         CLC   FLTUIWKS,RBUYDTWK   ERROR IF ACTUAL NUMBER OF WEEKS              
         BE    GETEDX              AND SPECIFIED NUMBER OF WEEKS DO NOT         
*                                  MATCH                                        
         LA    R0,EDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
GETEDX   DS    0H                                                               
         B     GETEX                                                            
         EJECT                                                                  
***********************************************************************         
* ADJUST THE NUMBER OF WEEKS BY CALCULATING THE ACTUAL NUMBER OF WEEKS          
* THE BUY HAS SPANNED                                                           
***********************************************************************         
ADJUSTWK NTR1                                                                   
         LA    R3,1                                                             
         MVC   WORK(6),EFFST                                                    
*                                                                               
ADJWK10  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK,F'7'                                        
         CLC   WORK(6),EFFEND                                                   
         BH    ADJWK20                                                          
         LA    R3,1(R3)                                                         
         B     ADJWK10                                                          
*                                                                               
ADJWK20  DS    0H                                                               
         STC   R3,RBUYDTWK                                                      
         B     EFFEXIT                                                          
         DROP  R6,R7                                                            
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053RECNT39   10/08/15'                                      
         END                                                                    
