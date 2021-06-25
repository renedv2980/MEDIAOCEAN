*          DATA SET SPSFM82    AT LEVEL 060 AS OF 09/20/07                      
*PHASE T21782A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21782A -- EQUIVALENCY RECORDS MAINTENANCE & LIST    *         
*                                                                     *         
*  COMMENTS:     MAINTAINS EQUIVALENCY RECORDS                        *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM40 (MAINT) & SCSFM41 (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21765 - EQUIVALENCY MAINTENANCE + LIST'                        
T21782   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1782**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       MVC   MYAGENCY,AGENCY                                                  
         CLC   =C'LIST',CONACT      WANT TO LIST?                               
         BE    *+8                  YES DISPSLN ONLY FO' MAINT ACTIONS          
         BAS   RE,DISPSLN           DISPLAY ALL SLN'S FROM SPSLNTAB             
         LA    R2,MNTMEDSH          R2 POINTER TO MEDIA FIELD HEADER            
         GOTO1 VALIMED              VALIDATE MEDIA                              
         MVC   MNTMEDE,MEDNM        DISPLAY MEDIA ON SCREEN                     
         OI    MNTMEDEH+6,X'80'     TRANSMIT                                    
         XC    TEMPCLT,TEMPCLT      TEMP CLIENT CODE                            
         LA    R2,MNTCLNTH          CLIENT HEADER                               
         CLI   5(R2),0              ANY INPUT FOR CLIENT?                       
         BE    VK20                 NO                                          
         CLC   =C'***',8(R2)        WANT DEFAULT RECORD?                        
         BNE   VK10                 NO                                          
         CLI   T217FFD+1,C'*'       DDS TERMINAL?                               
         BNE   VK10                 NO, LET VALICLT GIVE ERROR                  
         CLC   =C'LIST',CONACT      ACTION LIST?                                
         BE    VK10                 YES, LET VALICLT GIVE ERROR                 
         MVC   MYAGENCY,=C'00'      READ DEFAULT RECORD FOR DIS/CHA             
         B     VK20                                                             
*                                                                               
VK10     GOTO1 VALICLT              VALIDATE CLIENT                             
         MVC   TEMPCLT,BCLT         2 BYTE PACKED CLIENT CODE                   
*                                                                               
VK20     LA    R4,KEY                                                           
         USING EQUVD,R4                                                         
         XC    KEY,KEY              CLEAR THE KEY                               
         MVI   EQUKTYPE,X'09'       RECORD TYPE (X'09')                         
         MVC   EQUKAGY,MYAGENCY     AGENCY CODE                                 
         MVC   EQUKMED,QMED         MEDIA CODE                                  
         MVC   EQUKCLT(2),TEMPCLT   CLIENT CODE                                 
         DROP  R4                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       BAS   RE,DISPSLN           DISPLAY ALL SLN'S FROM SPSLNTAB             
         L     R6,AIO                                                           
         XC    FLAG,FLAG                                                        
         BAS   RE,SORTSLN                                                       
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR0                  YES                                         
*                                                                               
         MVI   ELCODE,X'09'         ELEMENT X'09'                               
         BAS   RE,GETEL             THIS ELEMENT IS REQUIRED                    
         BE    VR01                                                             
         DC    H'0'                                                             
VR0      LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
         USING EQUEL,R6                                                         
         MVI   EQUEL,X'09'          MOVE THE ELEMENT CODE IN                    
         MVI   EQUELEN,138          ELEMENT LENGTH = 138                        
*                                                                               
VR01     LA    R2,MNTS10H           FIRST DATA ON SCREEN                        
         LA    R5,MNTTABH           LAST FIELD ON SCREEN                        
         LA    R1,SLNSORT           SORTED SLN TABLE                            
*                                                                               
VR1      CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR2                  YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BZ    VR2                  NO                                          
VR1A     CLC   8(4,R2),=C'1000'     IF VAL=1000 SET SWITCH                      
         BNE   *+8                                                              
         OI    FLAG,XIST1000        VAL=1000                                    
         LLC   R3,0(R2)             ADVANCE TO NEXT INPUT SLOT                  
         AR    R2,R3                                                            
         LLC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         CR    R2,R5                END OF SCREEN?                              
         BNL   VR3                  YES                                         
         LA    R1,1(R1)             ADVANCE SORTED SLN TABLE PTR                
         CLI   0(R1),0              HIT END OF TABLE?                           
         BE    VR3                  YES                                         
         B     VR1                                                              
*                                                                               
VR2      CLI   5(R2),0              ANY INPUT?                                  
         BE    ERRMIS               NO...ERROR                                  
         TM    4(R2),X'08'          NUMERIC DATA?                               
         BZ    ERRNNM               NO                                          
*                                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'            INITIALIZE DUB TO ZERO (PACKED)             
         LLC   R3,5(R2)             GET LENGTH OF INPUT                         
         BCTR  R3,0                 SUBTRACT FOR EXECUTED PACK                  
         EX    R3,*+8               PACK INTO DUB                               
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB               CONVERT IN R0 TO BINARY                     
         LTR   R0,R0                ERROR?                                      
         BZ    ERRNNM               YES                                         
         STH   R0,HALF                                                          
         BAS   RE,EQUINDEX          GET EQUSECT1 INDEX IN R4                    
         MVC   0(2,R4),HALF         REPLACE FOR SOLO                            
         MVC   2(2,R4),HALF         AND PIGGYBACK                               
         B     VR1A                                                             
*                                                                               
VR3      LA    R2,MNTS10H           SET CURSOR TO FIRST SLOT                    
         TM    FLAG,XIST1000        NO VALUES OF 1000 PRESENT                   
         BZ    ERR1000                                                          
         MVC   EQUSECT2,EQUSECT1    REPLACE SECOND SET OF EQUIVS TOO            
         MVC   EQUDPT(2),=X'0001'   SET UP DAYPART POINTERS                     
         MVC   EQUDPT+2(14),EQUDPT+1                                            
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BNE   VRX                  NO                                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VRX      B     DR                   REDISPLAY RECORD                            
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       BAS   RE,CLRSCRN           CLEAR THE SCREEN                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'09'         ELEMENT X'09'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BE    *+6                  THIS ELEMENT IS REQUIRED                    
         DC    H'0'                                                             
         USING EQUEL,R6                                                         
         LA    R1,SLNSORT           SORTED SLN TABLE                            
         LA    R2,MNTS10H           FIRST FIELD TO DISPLAY                      
         LA    R3,MNTTAB            DISPLAY UP TO THIS FIELD                    
*                                                                               
DR10     CLI   0(R1),0              END OF SORTED SLN TABLE?                    
         BE    DRX                  YES                                         
*                                                                               
         BAS   RE,EQUINDEX          GET EQUSECT1 INDEX IN R4                    
         OC    0(2,R4),0(R4)        DO NOT DISPLAY A ZERO                       
         BNZ   DR15                                                             
*                                                                               
         BAS   RE,FIRST4                                                        
         BNE   DR12                                                             
*                                                                               
         MVC   8(4,R2),=C'1000'     DISPLAY 1000                                
         B     DR13                                                             
*                                                                               
DR12     BAS   RE,DIVSLN30                                                      
         L     R5,FULL                                                          
         EDIT  (R5),(4,8(R2)),0,ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
DR13     OI    4(R2),X'80'          ON CHANGE IT WILL NOT BE 0 ANYMORE!         
         OI    6(R2),X'80'          TRANSMIT                                    
         NI    4(R2),X'FF'-X'20'    MAKE SURE PREV VAL IS TURNED OFF            
         B     DR20                                                             
*                                                                               
DR15     EDIT  (2,0(R4)),(4,8(R2)),0,ZERO=NOBLANK,ALIGN=LEFT                    
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
DR20     LLC   R5,0(R2)             BUMP TO PROTECTED FIELD                     
         AR    R2,R5                                                            
         LLC   R5,0(R2)             BUMP TO NEXT DATA OR END OF SCREEN          
         AR    R2,R5                                                            
         LA    R1,1(R1)             NEXT SLN IN TABLE                           
         CR    R2,R3                IS END OF SCREEN?                           
         BL    DR10                 NO                                          
         DROP  R6                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       BAS   RE,DISPSLN           DISPLAY ALL SLN'S FROM SPSLNTAB             
         L     R4,AIO                                                           
         USING EQUVD,R4                                                         
         MVC   MNTMEDS,EQUKMED      DISPLAY MEDIA LETTER ON SCREEN              
         OI    MNTMEDSH+6,X'80'     TRANSMIT                                    
         MVC   MNTMEDE,MEDNM        DISPLAY MEDIA ON SCREEN                     
         OI    MNTMEDEH+6,X'80'     TRANSMIT                                    
         OC    EQUKCLT,EQUKCLT      IS THERE A CLIENT CODE?                     
         BNZ   DK10                                                             
         CLC   =C'00',EQUKAGY                                                   
         BNE   DKX                                                              
         MVC   MNTCLNT,=C'***'      DEFAULT CLIENT                              
         OI    MNTCLNTH+6,X'80'     TANSMIT                                     
         B     DKX                                                              
*                                                                               
DK10     MVC   TEMPCLT2,EQUKCLT     SAVE FOR CLIENT RECORD READ                 
         LA    R2,CLICODE           3 BYTE UNPACKED CLIENT CODE                 
         BAS   RE,GETCLINT          GET CLIENT RECORD FOR CLICODE               
         MVC   MNTCLNT,CLICODE      DISPLAY CLIENT                              
         OI    MNTCLNTH+6,X'80'     TANSMIT                                     
         DROP  R4                                                               
*                                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       LA    R4,KEY               R4 POINTER TO KEY                           
         MVI   NLISTS,10            10 DISPLAYS PER SCREEN                      
         OC    KEY,KEY              FIRST TIME THROUGH?                         
         BNZ   LR10                 NO                                          
*                                                                               
         BAS   RE,SORTSLN           SORT SLNTAB IN SLNSORT                      
*                                                                               
         USING EQUVD,R4                                                         
         MVI   EQUKTYPE,X'09'       X'09'                                       
         MVC   EQUKAGY,AGENCY       AGENCY CODE                                 
         MVC   EQUKMED,QMED         MEDIA CODE                                  
         MVC   EQUKCLT(2),TEMPCLT   CLIENT CODE                                 
         MVC   SAVEKEY,KEY          SAVE THE KEY                                
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE       SAME REC TYPE/AGENCY/MEDIA?                 
         BE    LR30                                                             
         CLI   T217FFD+1,C'*'       DDS TERMINAL?                               
         BNE   LRX                  NO, DO NOT SHOW DEFAULT!                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   EQUKTYPE,X'09'       X'09'                                       
         MVC   EQUKAGY,=C'00'       DEFAULT AGENCY CODE                         
         MVC   EQUKMED,QMED         MEDIA CODE                                  
         MVC   SAVEKEY,KEY          SAVE THE KEY                                
         DROP  R4                                                               
*                                                                               
LR10     GOTO1 HIGH                 FIRST RECORD                                
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON HIGH            
*                                                                               
LR20     LA    R4,KEY               R4 POINTER TO KEY                           
         GOTO1 SEQ                  NEXT RECORD                                 
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON SEQ             
*                                                                               
LR30     CLC   KEY(4),SAVEKEY       EQIVALENCY RECORD W SAME MEDIA/AGY?         
         BNE   LRX                  NO MORE DATATYPES TO LIST                   
*                                                                               
LR31     GOTO1 GETREC                                                           
         CLI   DMCB+8,0             DID WE GET THE RECORD?                      
         BE    *+6                  YES                                         
         DC    H'0'                 SHOULD ALWAYS GET A RECORD                  
*                                                                               
         MVC   LISTAR,SPACES        CLEAR THE LIST LINE                         
         L     R6,AIO                                                           
         USING EQUVD,R6                                                         
         XC    FLAG,FLAG            CLEAR NO CLIENT CODE FLAG                   
         MVC   TEMPCLT2,EQUKCLT     SAVE FOR CLIENT RECORD READ                 
         OC    EQUKCLT,EQUKCLT      IS THERE A CLIENT CODE?                     
         BNE   *+14                 YES                                         
         MVC   LSCLCODE(7),=C'DEFAULT'                                          
         OI    FLAG,NOCLIENT        SET NO CLIENT CODE FLAG                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'09'         THIS ELEMENT IS REQUIRED                    
         BAS   RE,GETEL                                                         
         BE    LR35                                                             
         DC    H'0'                                                             
*                                                                               
         USING EQUEL,R6                                                         
LR35     TM    FLAG,NOCLIENT        ANY CLIENT CODE?                            
         BNE   LR40                 NO                                          
         LA    R2,LSCLCODE          3 BYTE UNPACKED CLIENT CODE                 
         BAS   RE,GETCLINT          GET CLIENT RECORD AND DISPLAY               
         MVC   LSCLNAME,TEMPNAME    CLIENT NAME                                 
*                                                                               
** CALCULATE AND DISPLAY EQUIVALENCE FACTORS                                    
*                                                                               
LR40     GOTO1 LISTMON              DISP THE CLIENT CODE                        
         LA    R1,EQUSECT1          DAYPART SECTION 1 (FIRST HALFWORD)          
         LA    R2,SLNTAB            SLN TABLE                                   
*                                                                               
LR50     CLI   0(R2),0              END OF TABLE?                               
         BE    LR100                YES                                         
         CLC   0(2,R1),=H'1000'     CHECK GROUP                                 
         BNE   LR60                 GROUP OK                                    
         LA    R1,4(R1)             BUMP                                        
         LA    R2,1(R2)             BUMP SLN TABLE                              
         B     LR50                                                             
*                                                                               
LR60     BAS   RE,CNVEQU            CONVERT EQUIVALENCY AND DISPLAY             
*                                                                               
LR100    DS    0H                                                               
         B     LR20                 NEXT RECORD                                 
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           GETCLINT                                  *         
***********************************************************************         
GETCLINT NTR1                                                                   
         MVC   TEMPKEY,KEY          SAVE EQU RECORD'S KEY                       
         LA    R5,KEY               READ CLIENT HEADER                          
         USING CLTHDRD,R5           CLIENT RECORD DSECT                         
         XC    KEY,KEY              CLEAR KEY                                   
         MVC   CKEYAM,BAGYMD        AGENCY/MEDIA FROM SFM00 (VCLI)              
         MVC   CKEYCLT,TEMPCLT2     CLIENT CODE                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVC   DMCB+4(3),=X'D9000A' GET A(CLUNPK)                               
         MVI   DMCB+7,X'15'                                                     
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB              A(CLUNPK) IN RF                             
         GOTO1 (RF),(R1),CKEYCLT,0(R2)                                          
         MVI   LSCLCODE+3,C'/'      MOVE '/' TO SEPARATE CODE & NAME            
         MVC   TEMPNAME,CNAME       MOVE IN CLIENT NAME                         
         DROP  R5                                                               
*                                                                               
*RESTORE AS BEFORE READING CLIENT RECORD                                        
*                                                                               
         MVC   KEY(13),TEMPKEY      RESTORE EQU RECORD'S KEY                    
         GOTO1 READ                 READ THE EQU RECORD                         
         GOTO1 GETREC               GET THE EQU RECORD                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           CNVEQU                                    *         
***********************************************************************         
CNVEQU   NTR1                                                                   
*                                                                               
         LA    R1,SLNSORT           SORTED SLN TABLE                            
*                                                                               
CNV00    LA    R5,5                 NUMBER OF DISPLAYS ON SCREEN                
         LA    R4,LSHEAD            BEGIN PLACING SCREEN DATA HERE              
         MVC   LISTAR,SPACES                                                    
*                                                                               
CNV10    CLI   0(R1),0              END OF SLNSORT?                             
         BE    CNVX                 YES                                         
*                                                                               
         LLC   R2,0(R1)             GET SLN                                     
         EDIT  (R2),(3,0(R4))       PUT IT TO SCREEN                            
         MVI   3(R4),C'='                                                       
         LA    R4,4(R4)                                                         
*                                                                               
         LR    R0,R4                SAVE SCREEN PTR                             
         BAS   RE,EQUINDEX          GET EQUSECT1 INDEX IN R4                    
         OC    0(2,R4),0(R4)        DO NOT DISPLAY A ZERO                       
         BNZ   CNV15                                                            
         LR    R4,R0                RESTORE SCREEN PTR                          
         BAS   RE,FIRST4            FIRST 4 1000?                               
         BNE   CNV11                NO                                          
*                                                                               
         LA    R3,1000              DISPLAY 1000                                
         B     CNV25                                                            
*                                                                               
CNV11    BAS   RE,DIVSLN30                                                      
         L     R2,FULL                                                          
         CHI   R2,1000                                                          
         BNL   CNV21                                                            
         B     CNV16                                                            
*                                                                               
CNV15    CLC   0(2,R4),=H'1000'     ENTRY >=1000?                               
         BNL   CNV20                YES...DISPLAY FRACTION+WHOLE NUM            
         LH    R2,0(R4)             DISPLAY FRACTION                            
CNV16    LR    R4,R0                RESTORE SCREEN PTR                          
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(3,R4),DUB+6(2)                                                 
         MVI   1(R4),C'.'                                                       
         B     CNV30                                                            
*                                                                               
CNV20    LH    R2,0(R4)             DISPLAY WHOLE NUMBER + FRACTION             
CNV21    LR    R4,R0                RESORE SCREEN PTR                           
         SR    R3,R3                                                            
         MHI   R2,100                                                           
         SRDA  R2,31                                                            
         D     R2,=F'100'                                                       
         AHI   R3,1                                                             
         SRL   R3,1                                                             
         LR    R2,R5                                                            
CNV25    EDIT  (R3),(5,0(R4)),3     MOVE TO SCREEN (EDIT CLOBBERS R0)           
CNV30    LA    R4,6(R4)             NEXT SLOT ON SCREEN                         
         LA    R1,1(R1)             NEXT SLN                                    
         BCT   R5,CNV10             UNTIL END OF SCREEN                         
*                                                                               
         GOTO1 LISTMON                                                          
         B     CNV00                                                            
*                                                                               
CNVX     OC    LISTAR,LISTAR        DO WE HAVE ANYTHING FILLED OUT?             
         BZ    CNVXX                NOPE                                        
         GOTO1 LISTMON              YES, DISPLAY IT ON SCREEN                   
CNVXX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           FIRST4                                    *         
***********************************************************************         
FIRST4   NTR1                                                                   
         USING EQUEL,R6                                                         
         LA    R0,4                                                             
         LA    R4,EQUSECT1                                                      
*                                                                               
FRST4A   CLC   0(2,R4),=X'03E8'                                                 
         BNE   XIT                                                              
         LA    R4,4(R4)                                                         
         BCT   R0,FRST4A                                                        
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                           FIRST4                                    *         
***********************************************************************         
DIVSLN30 NTR1                                                                   
*                                                                               
         SR    R4,R4                                                            
         LLC   R5,0(R1)                                                         
         MHI   R5,100                                                           
         LA    R0,3                                                             
         DR    R4,R0                                                            
         ST    R5,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                           CLRSCRN                                   *         
***********************************************************************         
CLRSCRN  NTR1                                                                   
         LA    R2,MNTS10H           FIRST DATA ON SCREEN                        
         LA    R3,MNTTAB            CLEAR UP TO THIS FIELD                      
*                                                                               
CLR10    TM    1(R2),X'20'          IS IT PROTECTED?                            
         BO    CLR20                YES...JUST BUMP                             
         LLC   R1,0(R2)             HEADER + DATA                               
         SHI   R1,9                 SUBTRACT HEADER LENGTH +1                   
         TM    1(R2),X'02'          IS THERE AN EXTENDED FIELD HEADER?          
         BZ    *+8                  NO                                          
         SHI   R1,8                 SUBTRACT EXTENDED HEADER LENGTH             
*                                                                               
         EX    R1,*+8               PAD WITH BLANKS                             
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'          TRANSMIT                                    
*                                                                               
CLR20    LLC   R0,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R0                                                            
         CR    R2,R3                IS END OF SCREEN?                           
         BL    CLR10                NO                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           DISPSLN                                   *         
***********************************************************************         
DISPSLN  NTR1                                                                   
*                                                                               
         BAS   RE,SORTSLN           SORT SLN'S INTO SLNSORT                     
*                                                                               
         LA    R2,MNTSLN1H          FIRST SLN TO DISP                           
         LA    R3,MNTTAB            DISP UP TO THIS FIELD                       
         LA    R4,SLNSORT           SORTED SLN TABLE                            
*                                                                               
SLN10    TM    1(R2),X'20'          IS IT PROTECTED?                            
         BZ    SLN20                NO..JUST BUMP                               
*                                                                               
         CLI   0(R4),0              FINISHED ALL SLN'S?                         
         BE    SLNX                 YES, DONE                                   
         LLC   R5,0(R4)             GET SLN IN R5                               
         EDIT  (R5),(3,8(R2))                                                   
         OI    6(R2),X'80'          TRANSMIT                                    
         LA    R4,1(R4)             NEXT SLN                                    
*                                                                               
SLN20    LLC   R0,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R0                                                            
         CR    R2,R3                IS END OF SCREEN?                           
         BL    SLN10                NO                                          
*                                                                               
SLNX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*            SORT SLNTAB IN SLNSORT USING BUBBLE SORT                 *         
***********************************************************************         
SORTSLN  NTR1                                                                   
*                                                                               
         XC    SLNSORT,SLNSORT      CLEAR SORTED SLN TABLE                      
         MVC   SLNSORT(SLNTABQ-1),SLNTAB                                        
*                                                                               
         LA    R1,0                 START SORTING HERE (OUTER LOOP)             
         LA    R3,SLNTABQ-2         SORT THIS MANY SLN'S                        
*                                                                               
BSORT01  CR    R1,R3                SORTED THE WHOLE THING?                     
         BNL   BSORTX               YES,DONE                                    
         LA    R2,0                 START SORTING HERE (INNER LOOP)             
*                                                                               
BSORT02  LR    R4,R3                TOTAL SLN'S TO SORT                         
         SR    R4,R1                MINUS THIS INDEX                            
         CR    R2,R4                INNER LOOP UP TO OUTER LOOPS INDEX?         
         BNL   BSORT04              YES, BUMP OUTER LOOP                        
*                                                                               
         LA    R4,SLNSORT           SLN TABLE WE ARE SORTING                    
         LA    R4,0(R2,R4)          UP TO THIS INDEX                            
         CLC   1(1,R4),0(R4)        RIGHT NUM >= LEFT NUM?                      
         BNL   BSORT03              YES, NO NEED TO SWAP THEM                   
*                                                                               
         MVC   BYTE,0(R4)           SWAP 'EM                                    
         MVC   0(1,R4),1(R4)                                                    
         MVC   1(1,R4),BYTE                                                     
*                                                                               
BSORT03  LA    R2,1(R2)             BUMP INNER LOOP INDEX                       
         B     BSORT02                                                          
*                                                                               
BSORT04  LA    R1,1(R1)             BUMP OUTER LOOP INDEX                       
         B     BSORT01                                                          
*                                                                               
BSORTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* POINT R4 TO PROPER SLN IN EQUSECT1 BASED ON SORTED INDEX            *         
***********************************************************************         
EQUINDEX NTR1                                                                   
         USING EQUEL,R6                                                         
         LA    R2,SLNTAB            UNSORTED SLN TABLE                          
         LA    R3,0                 COUNTER                                     
*                                                                               
EQU05    CLI   0(R2),0              REACHED END OF TABLE?                       
         BNE   *+6                  NO, SHOULD NEVER                            
         DC    H'0'                                                             
         CLC   0(1,R2),0(R1)        DISP THIS SLN?                              
         BE    EQU10                YES                                         
         LA    R2,1(R2)             BUMP UNSORTED SLN TABLE                     
         AHI   R3,1                 BUMP COUNTER                                
         B     EQU05                                                            
*                                                                               
EQU10    LA    R4,EQUSECT1          SLN IN REC                                  
         CHI   R3,0                 WANT DO DISP FIRST SLN IN REC?              
         BE    EQUX                 YES                                         
         LA    R4,4(R4)             NEXT SLN IN REC                             
         BCT   R3,*-4                                                           
*                                                                               
EQUX     XIT1  REGS=(R4)            DO NOT RESTORE R4                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                          SLN TABLE                                  *         
***********************************************************************         
SLNTAB   DS    0XL1                                                             
       ++INCLUDE SPSLNTAB                                                       
         DC    AL1(0)                                                           
SLNTABX  EQU   *                                                                
SLNTABQ  EQU   SLNTABX-SLNTAB                                                   
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERR1000  MVC   ERRNUM,=AL2(170)     NO VALUES OF 1000 INPUT                     
         B     SPERREX                                                          
ERRMIS   MVC   ERRNUM,=AL2(1)       MISSING INPUT                               
         B     SPERREX                                                          
ERRNNM   MVC   ERRNUM,=AL2(3)       NUMERIC DATA ONLY                           
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*SPGENEQU   (EQUIVALENCY RECORDS)                                               
*SPSFMFFD                                                                       
*SCSFM40D   (M SCREEN)                                                          
*SCSFM41D   (L SCREEN)                                                          
*DDGENTWA                                                                       
*SPGENCLT   (CLIENT RECORDS)                                                    
*FAGETTXTD  (ERROR MESSAGES)                                                    
*SPSFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
EQUVD    DSECT                                                                  
       ++INCLUDE SPGENEQU          EQUIVALENCY RECORDS                          
EQUKTYPQ EQU   X'09'                                                            
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM40D          MAINTENACE SCREEN                            
         EJECT                                                                  
       ++INCLUDE SCSFM41D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD                                
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                    RELOCATION FACTOR                           
*                                                                               
ERRNUM   DS    XL2                  FOR ERRORS                                  
TEMPCLT  DS    CL2                  CLIENT FROM LIST SCREEN                     
TEMPCLT2 DS    CL2                  CLIENT FROM EQU REC TO CLT REC              
TEMPNAME DS    CL20                 TEMP CLIENT NAME                            
CLICODE  DS    CL3                  CLIENT NAME SAVED FROM LR TO DK             
*                                                                               
FLAG     DS    X                    VARIOUS FLAGS                               
XIST1000 EQU   X'80'                FOUND 1000 VALUE IN DAYPART SECTION         
NOCLIENT EQU   X'40'                NO CLIENT ("DEFAULT" IN LIST)               
*                                                                               
SAVEKEY  DS    CL13                 CHECKS RECORD AFTER DMRDHI                  
TEMPKEY  DS    CL13                 SAVES KEY WHILE READING CLIENT REC          
SLNSORT  DS    CL(SLNTABQ)          SORTED SLN'S                                
MYAGENCY DS    CL2                  IN CASE WE NEED DEFAULT C'00' AGY           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR               LABELS FOR LISTMON                          
LSCLCODE DS    CL3                  CLIENT CODE                                 
         DS    CL1                  /                                           
LSCLNAME DS    CL20                 CLIENT NAME                                 
         ORG   LSCLCODE             OR...                                       
LSHEAD   DS    CL4                  10=....                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPSFM82   09/20/07'                                      
         END                                                                    
