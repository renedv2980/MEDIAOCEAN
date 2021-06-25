*          DATA SET CTSFM58    AT LEVEL 067 AS OF 05/31/17                      
*PHASE TA0A58A                                                                  
         TITLE 'TA0A58  DARE FEATURE RECORD'                                    
**** CHANGE LOG                                                                 
*                                                                               
* BOBY   08/12  BIG BANG                                                        
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        TA0A58 - DARE FEATURE RECORD MAINT/LIST/REPORT        *         
*                                                                     *         
*  CALLED FROM  GENCON VIA TA0A00 (SFM CTFILE CONTROLLER)             *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN TA0A98 (MAINTENANCE)                           *         
*               SCREEN TA0A99 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED DARE FEATURE RECORD                           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - INIT'                             
TA0A58   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A58                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,XRECADD        ID FILE CHANGED-RECORD ADDED                 
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD CHANGED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD DELETED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD RESTORED                           
         BE    *+8                                                              
         B     CKMODE20                                                         
*                                                                               
         OI    GENSTAT2,USGETTXT+USMYOK USE GETTXT AND MY MSG                   
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVI   GTMTYP,GTMINF       INFORMATIONAL MESSAGE                        
*                                                                               
         LA    RF,RECYCLEQ         RECYCLE MESSAGE                              
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         B     CKMODEX                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
CKMODE20 DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         GOTOR ERREX                                                            
*                                                                               
INITERR  DS    0H                                                               
         LA    R2,CONACTH           SECURITY LOCKOUT                            
         MVI   ERROR,SECLOCK                                                    
         GOTOR ERREX                                                            
*                                                                               
NOCHG    EQU   79                  FIELD CANNOT BE CHANGED                      
NOTOT    EQU   84                  DATA TYPE CANNOT BE TOTALLED                 
INVLEN   EQU   121                 LENGTH INVALID FOR DATA TYPE                 
NOSHRT   EQU   122                 LENGTH CANNOT BE DECREASED                   
NODEC    EQU   168                 DATA TYPE CANNOT HAVE DECIMALS               
STDID    EQU   602                 STANDARD CODE STARTS WITH !                  
CCLID    EQU   603                 CUSTOM   CODE CAN'T START WITH !             
*                                                                               
RECYCLEQ EQU   164                 CALL COMPUTER ROOM TO RECYCLE                
*                                                                               
CKMODEX  DS    0H                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - VL'                               
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVCTEFKY,SVCTEFKY        CLEAR                                   
         LA    R4,SVCTEFKY                                                      
*                                                                               
         USING CTEFRECD,R4         ESTABLISH FEATURE RECORD                     
*                                                                               
         MVI   CTEFKTYP,CTEFKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEFKSTY,CTEFKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BNE   VKLISTN                                                          
*                                                                               
         XC    SVFTR#,SVFTR#       SAVE FEATURE NUMBER SAVEAREA                 
*                                                                               
         CLI   SCLFTR#H+5,0        OKAY IF NOT ENTERED                          
         BE    VK90                                                             
*                                                                               
         LLC   RF,SCLFTR#H+5       GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SCLFTR#(0)   PACK ENTERED NUMBER                          
*                                                                               
         TP    DUB                 SKIP IF NOT PACKED NUMBER                    
         BNZ   VK90                                                             
*                                                                               
         CVB   RF,DUB              CVB                                          
         STCM  RF,3,CTEFKNUM       ADD TO KEY                                   
         STCM  RF,3,SVFTR#         SAVE ID                                      
*                                                                               
         B     VK90                                                             
*                                                                               
VKLISTN  DS    0H                                                               
*                                                                               
         LA    R2,SCRFTR#H         POINT TO FEATURE NUMBER                      
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING NEW FEATURE                        
         BNE   VK05                                                             
*                                                                               
         BRAS  RE,NXTFTR#             FIND NEXT AVAILABLE FEATURE NUM           
*                                                                               
         LA    RF,SCRFTR#                                                       
*******  FOUT  SCRFTR#H,SPACES,(RF) INIT FIELD                                  
*                                                                               
         LH    RF,SVFTR#           GET FOUND FEATURE NUMBER                     
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  8(3,R2),DUB         DISPLAY NEW NUMBER                           
         MVI   5(R2),3             SET FIELD LENGTH                             
*                                                                               
         FOUT  SCRFTR#H                                                         
*                                                                               
         B     VK20                                                             
*                                                                               
VK05     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTDIS       FOR DISPLAY                                  
         BE    *+8                                                              
         CLI   ACTNUM,ACTCHA       FOR CHANGE                                   
         BE    *+8                                                              
         CLI   ACTNUM,ACTSEL       FOR SELECT                                   
         BNE   VKX                                                              
*                                                                               
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK10                YES                                          
*                                  NO                                           
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VK10     DS    0H                  VALIDATE NUMBER INPUT                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)            INPUT LENGTH                                 
         CHI   RF,3                MAX LENGTH OF THREE                          
         BH    VKNINV                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         PACK INPUT                                   
*                                                                               
         TP    DUB                 TEST FOR PACKED NUMBER                       
         BNZ   VKNINV              NOT NUMERIC                                  
*                                                                               
         CVB   RF,DUB              CVB                                          
*                                                                               
         CH    RF,SVFTR#           OKAY IF NUMBER NOT CHANGED                   
         BE    VK20                                                             
*                                                                               
         STH   RF,SVFTR#           SAVE NEW FEATURE #                           
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BNE   VK20                                                             
*                                                                               
         MVI   ACTNUM,ACTDIS          REVERT TO ACTION DISPLAY                  
*                                                                               
VK20     DS    0H                                                               
*                                                                               
         MVC   CTEFKNUM,SVFTR#     ADD NUMBER TO KEY                            
*                                                                               
VK90     DS    0H                                                               
*                                                                               
         MVC   KEY,CTEFKEY         SET KEY                                      
*                                                                               
*****    GOTOR HIGH                RESET FILE POINTERS                          
*                                                                               
VKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKMISS   LA    RF,MISSING          REQUIRED FIELD                               
         B     VKERR                                                            
*                                                                               
VKNINV   LA    RF,INVALID          INVALID INPUT                                
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         TITLE 'CTSFM22 - DARE FEATURE MAINTENANCE - NXTFTR#'                   
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE FEATURE #                *         
*                                                                     *         
*        IF THERE ARE NO DARE FEATURE RECORD ON FILE                  *         
*           START NUMBERING AT 1                                      *         
*                                                                     *         
*        ROUTINE READS FILE FOR FEATURE RECORDS SAVING LAST ONE READ  *         
*           ON CHANGE OF RECORD TYPE, # FROM LAST ONE READ IS         *         
*           BUMPED BY ONE AND RETURNED TO CALLER                      *         
*                                                                     *         
*EXIT    QSQN#  =  FOUND NEW SEQUENCE NUMBER                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NXTFTR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         MVC   SVCTEFKY,KEY        SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS FEATURE RECORD KEY          
         USING CTEFKEY,R4                                                       
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVI   CTEFKTYP,CTEFKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEFKSTY,CTEFKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST RECORD ON FILE                
*                                                                               
NXTFT#LP DS    0H                                                               
*                                                                               
         CLC   CTEFKEY(CTEFKNUM-CTEFKEY),KEYSAVE   DONE IF KEY CHANGE           
         BNE   NXTFT#DN                                                         
*                                                                               
NXTFT#CN DS    0H                                                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
*                                                                               
         B     NXTFT#LP                                                         
*                                                                               
NXTFT#DN DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CTEFKNUM       GET LAST FEATURE # ON FILE                   
         AHI   RF,1                BUMP BY ONE                                  
         STCM  RF,3,SVFTR#         RETURN NEXT AVAILABLE FETURE #               
*                                                                               
         MVC   KEY,SVCTEFKY        RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
NXTFTR#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - DK'                               
***********************************************************************         
*                                                                     *         
*     DISPLAY  KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING CTEFRECD,R4         ESTABLISH FEATURE RECORD                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CTEFKNUM       GET FEATURE NUMBER                           
*                                                                               
         STH   RF,SVFTR#           SET SAVED FEATURE NUMBER                     
*                                                                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FULL(3),DUB         DISPLAY FEATURE NUMBER                       
*                                                                               
         FOUT  SCRFTR#H,FULL,3                                                  
*                                                                               
DKX      DS    0H                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - VR'                               
***********************************************************************         
*                                                                     *         
*     VALIDATE RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCTEFKY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         L     R4,AIO                                                           
         USING CTEFRECD,R4         ESTABLISH FOUND RECORD                       
*                                                                               
         XC    CTEFRECD(100),CTEFRECD   INIT RECORD BUILD AREA                  
         MVC   CTEFKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,CTEFDAT-CTEFKEY  MINIMUM LENGTH OF RECORD                     
*                                                                               
         STCM  RF,3,CTEFFLEN       SET MINIMUM RECORD LENGTH                    
*                                                                               
*        VALIDATE FIELD NAME                                                    
*                                                                               
         LA    R6,CTEFDAT          POINT TO FIRST ELEMENT                       
         USING CTFNMD,R6           ESTABLISH FEATURE NAME ELEMENT               
*                                                                               
         MVI   CTFNMEL,CTFNMELQ    SET ELEMENT ID                               
*                                                                               
         LA    R2,SCRFTRNH         POINT TO FEATURE NAME FLD                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS              MUST BE ENTERED                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTFNM(0),8(R2)      SAVE FEATURE NAME                            
*                                                                               
         LA    RF,1+(CTFNM-CTFNMD)(RF) ELEMENT LENGTH                           
         STC   RF,CTFNMLEN         SET IN ELEMENT                               
*                                                                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
*                                                                               
*        VALIDATE FIELD DESCRIPTION                                             
*                                                                               
         LA    R0,1                NUMBER OF DESCRIPTION FIELDS                 
         LA    R2,SCRDSC1H         POINT TO FIRST DESCRIPTION ELEM              
*                                                                               
VRDSCLP  DS    0H                                                               
*                                                                               
         USING CTFDSD,R6           ESTABLISH FEATURE DESCRIPTION ELEM           
*                                                                               
         MVI   CTFDSEL,CTFDSELQ    SET ELEMENT ID                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VRDSC10                                                          
*                                                                               
         CHI   R0,1                IF FIRST DESCRIPTION FIELD                   
         BE    VRMISS                 MUST BE ENTERED                           
*                                                                               
         LHI   RF,1                FORCE 1 BYTE TO DESCRIPTION                  
         MVI   CTFDS,C' '                                                       
*                                                                               
         B     VRDSC90                                                          
*                                                                               
VRDSC10  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTFDS(0),8(R2)      SAVE FEATURE NAME                            
*                                                                               
         AHI   RF,1                RESTORE LENGTH                               
*                                                                               
VRDSC90  DS    0H                                                               
*                                                                               
         LA    RF,(CTFDS-CTFDSD)(RF) ELEMENT LENGTH                             
         STC   RF,CTFDSLEN         SET IN ELEMENT                               
*                                                                               
VRDSCCN  DS    0H                                                               
*                                                                               
         LA    R2,(SCRDSC2H-SCRDSC1H)(R2) BUMP TO NEXT DESC FLD                 
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         AHI   R0,1                BUMP ELEMENT NUMBER                          
*                                                                               
         CHI   R0,5                MAX FIVE FIELDS                              
         BNH   VRDSCLP                                                          
*                                                                               
VRDSCDN  DS    0H                                                               
*                                                                               
*        VALIDATE TEXT MESSAGE IDS AND CONTEXT                                  
*                                                                               
VRMSG    DS    0H                                                               
*                                                                               
         LHI   R0,1                INIT LINE COUNTER                            
         LA    R2,SCRTXT1H         POINT TO FIRST FEATURE TEXT FLD              
         SR    RF,RF                                                            
*                                                                               
VRMSGLP  DS    0H                                                               
*                                                                               
         USING CTMSGD,R6           ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         XC    CTMSGEL(CTMSGLNQ),CTMSGEL   INIT ELEMENT                         
*                                                                               
         MVI   CTMSGEL,CTMSGELQ    SET ELEMENT ID                               
         MVI   CTMSGLEN,CTMSGLNQ   SET ELEMENT LENGTH                           
*                                                                               
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VRMSG10             OKAY IF SOMETHING THERE                      
*                                                                               
         CHI   R0,1                IF FIRST TEXT FIELD                          
         BE    VRMISS                 MUST BE ENTERED                           
*                                                                               
         LA    R1,SCRCTX1H-SCRTXT1H(R2) CONTEXT ON SAME LINE                    
         CLI   5(R1),0             NO CONTEXT ALLOWED IF NO TEXT                
         BNE   VRINV                                                            
*                                                                               
         B     VRMSGCN             NEXT LINE                                    
*                                                                               
VRMSG10  DS    0H                                                               
*                                                                               
         CHI   RF,6                MUST BE 6 LONG                               
         BNE   VRINV                                                            
*                                                                               
*        VALIDATE TEXT ID AGAINST TABLE                                         
*                                                                               
         LA    R1,DARERECS         POINT TO VALID REC IDS                       
*                                                                               
VRTXTLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         ERROR IF EOT REACHED                         
         BE    VRINV                                                            
*                                                                               
         CLC   0(6,R1),8(R2)       FIND TEXT ID IN TABLE                        
         BE    VRTXTFD                                                          
*                                                                               
VRTXTCN  DS    0H                                                               
*                                                                               
         LA    R1,6(R1)            BUMP TO NEXT REC IN TABLE                    
         B     VRTXTLP                                                          
*                                                                               
VRTXTFD  DS    0H                                                               
*                                                                               
         MVC   CTMSGTXT,8(R2)      SAVE TEXT ID                                 
*                                                                               
*        VALIDATE CONTEXT AGAINST TABLE                                         
*                                                                               
         LR    R3,R2               SAVE A(TEXT FIELD)                           
*                                                                               
         LA    R2,(SCRCTX1H-SCRTXT1H)(R2)     CONTEXT FIELD                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRMISS                 MUST BE ENTERED                           
*                                                                               
         CHI   RF,6                MUST BE 6 LONG                               
         BNE   VRINV                                                            
*                                                                               
*        VALIDATE CONTEXT ID AGAINST TABLE                                      
*                                                                               
         LA    R1,DARERECS         POINT TO VALID REC IDS                       
*                                                                               
VRCTXLP  DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         ERROR IF EOT REACHED                         
         BE    VRINV                                                            
*                                                                               
         CLC   0(6,R1),8(R2)       FIND TEXT ID IN TABLE                        
         BE    VRCTXFD                                                          
*                                                                               
VRCTXCN  DS    0H                                                               
*                                                                               
         LA    R1,6(R1)            BUMP TO NEXT REC IN TABLE                    
         B     VRCTXLP                                                          
*                                                                               
VRCTXFD  DS    0H                                                               
*                                                                               
         MVC   CTMSGCTX,8(R2)      SAVE CONTEXT ID                              
*                                                                               
         LR    R2,R3               RESTORE A(TEXT FIELD)                        
*                                                                               
         LA    R6,CTMSGLNQ(R6)     NEXT ELEMENT IN RECORD                       
*                                                                               
VRMSGCN  DS    0H                                                               
*                                                                               
         LA    R2,(SCRTXT2H-SCRTXT1H)(R2) BUMP TO NEXT TEXT FLD                 
*                                                                               
         AHI   R0,1                BUMP LINE COUNTER                            
*                                                                               
         CHI   R0,((SCRTXTLH-SCRTXT1H)/(SCRTXT2H-SCRTXT1H))                     
         BNH   VRMSGLP             NEXT LINE                                    
*                                                                               
VRMSGDN  DS    0H                                                               
*                                                                               
VRMSGX   DS    0H                                                               
*                                                                               
         MVI   0(R6),0             FORCE TRAILING NULL                          
         SR    R6,R4               RECORD LENGTH                                
         STCM  R6,3,CTEFFLEN       SET RECORD LENGTH                            
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         BRAS  RE,DR               DISPLAY RECORD                               
*                                                                               
         XIT1                                                                   
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
         B     VRERR                                                            
*                                                                               
VRMISS   MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
DARERECS DS    0D                  VALID DARE MESSAGE RECORDS                   
         DC    C'AGYHDR'                                                        
         DC    C'AGYDS1'                                                        
         DC    C'AGYDS2'                                                        
         DC    C'AGYCDC'           ORDER'S COMSCORE DEMO CATEGORIES             
         DC    C'AGYTDM'           TARGET DEMO CATEGORIES                       
         DC    C'AGYDS3'                                                        
         DC    C'AGYDS4'                                                        
         DC    C'AGYDS5'                                                        
         DC    C'AGYSAL'                                                        
         DC    C'AGYSTD'                                                        
         DC    C'AGYCOM'                                                        
         DC    C'AGYHIA'                                                        
         DC    C'AGYCAN'                                                        
         DC    C'BUYNWK'                                                        
         DC    C'BUYHDR'                                                        
         DC    C'BUYDEM'                                                        
         DC    C'BUYDM2'                                                        
         DC    C'BUYDV2'           ALL DEMO VALUES AT 2 DECIMALS                
         DC    C'BUYCDV'           ORDER'S COMSCORE DEMO VALUES                 
         DC    C'BUYORB'                                                        
         DC    C'BUYCOM'                                                        
         DC    C'BUYAAU'           AUTOMATED AVAILS UUID                        
         DC    C'BUYDTL'                                                        
         DC    C'AGYTLR'                                                        
         DC    C'AGYRCL'                                                        
         DC    C'ERRNOT'                                                        
         DC    C'DLNNOT'                                                        
         DC    C'ORDAPP'                                                        
         DC    C'ORDREJ'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDTLR'                                                        
         DC    C'ORDCFM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDRCL'                                                        
         DC    C'MKGHDR'                                                        
         DC    C'MKGDS1'                                                        
         DC    C'MKGDS2'                                                        
         DC    C'MKGCDC'           MKGD'S COMSCORE DEMO CATEGORIES              
         DC    C'MKGDC2'           ALL DEMO VALUES AT 2 DECIMALS                
         DC    C'MKGMNW'                                                        
         DC    C'MKGMSS'                                                        
         DC    C'MKGONW'                                                        
         DC    C'MKGBUY'                                                        
         DC    C'MKGDEM'                                                        
         DC    C'MKGCDV'           MKGD'S COMSCORE DEMO VALUES                  
         DC    C'MKGORB'                                                        
         DC    C'MKGCOM'                                                        
         DC    C'MKGDTL'                                                        
         DC    C'MKGTLR'                                                        
         DC    C'MKGAPP'                                                        
         DC    C'MKGREJ'                                                        
         DC    C'MKGRCM'                                                        
         DC    C'MKGROK'                                                        
         DC    C'MKGCAN'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDURL'                                                        
         DC    C'ORDMO1'                                                        
         DC    C'AGYTST'                                                        
         DC    C'BUYTST'                                                        
         DC    C'MKGTST'                                                        
         DC    C'MKGACM'                                                        
         DC    C'MK2TLR'                                                        
         DC    C'VARHDR'       THE VAR RECORDS ARE RARELY IF EVER USED          
         DC    C'VARDS1'         SO PUTTING THESE AT THE END OF THE             
         DC    C'VARDS2'         TABLE                                          
         DC    C'VARDS3'                                                        
         DC    C'VARDS4'                                                        
         DC    C'VARDS5'                                                        
         DC    C'VARSTD'                                                        
         DC    C'VARCOM'                                                        
         DC    C'VARPRD'                                                        
         DC    C'VARTLR'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
         TITLE 'TA0A58  DARE FEATURE RECORD - DR'                               
***********************************************************************         
*                                                                     *         
*     DISPLAY  RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING CTEFRECD,R4         ESTABLISH FEATURE RECORD                     
*                                                                               
*        DISPLAY FEATURE NAME                                                   
*                                                                               
DRNAM    DS    0H                                                               
*                                                                               
         MVI   ELCODE,CTFNMELQ     FIND FEATURE NAME ELEMENT                    
         LR    R6,R4               POINT TO START OF RECORD                     
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNE   DRNAMX                                                           
*                                                                               
         USING CTFNMD,R6           ESTABLISH FEATURE NAME ELEMENT               
*                                                                               
         FOUT  SCRFTRNH,SPACES,25     CLEAR FIELD                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CTFNMLEN         ELEMENT LENGTH                               
         SHI   RF,(CTFNM-CTFNMD)   LENGTH OF NAME                               
         FOUT  SCRFTRNH,CTFNM,(RF)                                              
*                                                                               
DRNAMX   DS    0H                                                               
*                                                                               
*        CLEAR DESCRIPTION FIELDS                                               
*                                                                               
DRDCLR   DS    0H                                                               
*                                                                               
         LA    R0,(SCRDSC5H-SCRDSC1H)/(SCRDSC2H-SCRDSC1H) MAX DSC FLDS          
         LA    R2,SCRDSC1H         POINT TO FIRST FEATURE DESC ELM              
*                                                                               
DRDCLRLP LA    RF,L'SCRDSC1        LENGTH OF DESCRIPTION FLD                    
         FOUT  (R2),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         LA    R2,(SCRDSC2H-SCRDSC1H)(R2) BUMP TO NEXT DESC FLD                 
         BCT   R0,DRDCLRLP         DISPLAY NEXT DESCRIPTION FIELD               
*                                                                               
*        DISPLAY DESCRIPTION ELEMENTS                                           
*                                                                               
DRDSC    DS    0H                                                               
*                                                                               
         LA    R0,(SCRDSC5H-SCRDSC1H)/(SCRDSC2H-SCRDSC1H) MAX DSC FLDS          
         LA    R2,SCRDSC1H         POINT TO FIRST FEATURE DESC ELM              
*                                                                               
         MVI   ELCODE,CTFDSELQ     FIND FEATURE DESCRIPTION ELEMENT             
         LR    R6,R4               POINT TO START OF RECORD                     
         BRAS  RE,GETEL            FIND ELEMENT                                 
DRDSCLP  BNE   DRDSCDN             NONE LEFT                                    
*                                                                               
         USING CTFDSD,R6           ESTABLISH DESCRIPTION ELEMENT                
         SR    RF,RF                                                            
         IC    RF,CTFDSLEN         GET ELEMENT LENGTH                           
         SHI   RF,CTFDS-CTFDSD     FIELD LENGTH                                 
         FOUT  (R2),CTFDS,(RF)     DISPLAY FIELD                                
         BCT   R0,*+8              NEXT DECRIPTION FIELD                        
         B     DRDSCDN             NO MORE ROOM ON SCREEN                       
*                                                                               
         LA    R2,(SCRDSC2H-SCRDSC1H)(R2) BUMP TO NEXT DESC FLD                 
         BRAS  RE,NEXTEL           FIND NEXT DESCRIPTION ELEMENT                
         B     DRDSCLP             DISPLAY IT                                   
*                                                                               
DRDSCDN  DS    0H                                                               
*                                                                               
*        CLEAR TEXT MESSAGES DATA FOR FEATURE                                   
*                                                                               
         LA    R0,(SCRTXTLH-SCRTXT1H)/(SCRTXT2H-SCRTXT1H) MAX TXT MSGS          
         LA    R2,SCRTXT1H         POINT TO FIRST FEATURE MSG ELM               
*                                                                               
DRCLRLP  LA    RF,L'SCRTXT1        LENGTH OF MESSAGE FLD                        
         FOUT  (R2),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         LA    R3,(SCRCTX1H-SCRTXT1H)(R2)   CONTEXT FIELD                       
         LA    RF,L'SCRCTX1        LENGTH OF MESSAGE CONTEXT FLD                
         FOUT  (R3),SPACES,(RF)    CLEAR FIELD                                  
*                                                                               
         LA    R2,(SCRTXT2H-SCRTXT1H)(R2)        NEXT MESSAGE                   
         BCT   R0,DRCLRLP                                                       
*                                                                               
DRCLRDN  DS    0H                                                               
*                                                                               
*        DISPLAY TEXT MESSAGE IDS AND CONTEXT                                   
*                                                                               
DRMSG    DS    0H                                                               
*                                                                               
         LA    R0,(SCRTXTLH-SCRTXT1H)/(SCRTXT2H-SCRTXT1H) MAX TXT MSGS          
         LA    R2,SCRTXT1H         POINT TO FIRST FEATURE DESC ELM              
         SR    RF,RF                                                            
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         MVI   ELCODE,CTMSGELQ     LOOK FOR MESSAGE ELEMENT                     
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
DRMSGLP  BNE   DRMSGDN             NO MORE ELEMENTS                             
*                                                                               
         USING CTMSGD,R6           ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         LA    R3,L'CTMSGTXT       TEXT FIELD LENGTH                            
         FOUT  (R2),CTMSGTXT,(R3)  DISPLAY FIELD                                
*                                                                               
         LA    R5,(SCRCTX1H-SCRTXT1H)(R2) CONTEXT FIELD                         
*                                                                               
         LA    R3,L'CTMSGCTX       CONTEXT FIELD LENGTH                         
         FOUT  (R5),CTMSGCTX,(R3)  DISPLAY FIELD                                
*                                                                               
         LA    R2,(SCRTXT2H-SCRTXT1H)(R2) BUMP TO NEXT TEXT FLD                 
         BCT   R0,*+8              NEXT TEXT LINE                               
         B     DRMSGDN             NO MORE LINES LEFT                           
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT MESSAGE ELM                        
         B     DRMSGLP                                                          
*                                                                               
DRMSGDN  DS    0H                                                               
*                                                                               
*        DISPLAY ACTIVITY                                                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         MVI   ELCODE,X'F1'        LOOK FOR ACTIVITY ELEMENT                    
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
         BNE   DRACTVX             NONE                                         
*                                                                               
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
*                                                                               
DRACTVX  XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(14,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - XR'                               
***********************************************************************         
*                                                                     *         
*     AFTER RECORD ADDED ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
XRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A58  DARE FEATURE RECORD - LR'                               
***********************************************************************         
*                                                                     *         
*     LIST RECORDS ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
*                                                                               
         USING CTEFKEY,R4          ESTABLISH FEATURE KEY                        
*                                                                               
         MVI   CTEFKTYP,CTEFKTYQ   SET RECORD MAJOR TYPE                        
         MVI   CTEFKSTY,CTEFKSTQ   SET RECORD SECONDARY TYPE                    
*                                                                               
         MVC   CTEFKNUM,SVFTR#     SET ANY STARTING NUMBER                      
*                                                                               
LR010    GOTO1 HIGH                                                             
*                                                                               
         B     LR030                                                            
*                                                                               
LR020    GOTO1 HIGH                RE-POINT FILE                                
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,CTEFDAT          POINT TO FIRST ELEMENT                       
*                                                                               
         USING CTFNMD,R6           ESTABLISH FEATURE NAME ELM                   
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CTEFKNUM       GET FEATURE NUMBER                           
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  LISTFTR#,DUB        DISPLAY FEATURE NUMBER                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CTFNMLEN         GET ELEMENT LENGTH                           
         SHI   RF,(CTFNM-CTFNMD)   LENGTH OF NAME                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISTNAM(0),CTFNM    DISPLAY FEATURE NAME                         
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM99D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9AD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVFTR#   DS    H                   FEATURE NUMBER SAVEAREA                      
LUPDATE  DS    CL14                BUFFER FOR DATE/TIME OF LAST UPDATE          
X        DS    XL100                                                            
*                                                                               
SVCTEFKY DS    XL32                KEY SAVEAREA                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LISTFTR# DS    CL3                                                              
         DS    CL3                                                              
LISTNAM  DS    CL25                                                             
         DS    CL1                                                              
         EJECT                                                                  
* GEGENEDI                                                                      
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDCOMFACS                                                                     
* DDFLDIND                                                                      
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDACTIVD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067CTSFM58   05/31/17'                                      
         END                                                                    
