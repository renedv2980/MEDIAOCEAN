*          DATA SET SPSFM64    AT LEVEL 002 AS OF 06/02/10                      
*PHASE T21764A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        T21764  -- DAYPART SPLIT RECORD MAINTENANCE          *         
*                                                                     *         
*  COMMENTS:     MAINTAINS DAYPART SPLIT RECORDS                      *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM5E (MAINT) & SPSFM5D (LIST)              *         
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
         TITLE 'T21764 - DAYPART SPLIT MAINTENANCE AND LIST'                    
T21764   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1764**,R7,RR=R3                                              
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
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY?                         
         BE    VK                  YES                                          
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                  YES                                          
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)?                      
         BE    DK                  YES                                          
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                  YES                                          
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                  YES                                          
*                                                                               
XIT      XIT1                      EXIT                                         
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       CLI   ACTEQU,ACTADD       IS THE ACTION ADD?                           
         BNE   VK00                NO                                           
         TM    SPLMEDKH+4,X'20'    MEDIA WAS PREVIOUSLY VALIDATED?              
         BZ    VK00                NO - GO VALIDATE                             
         TM    SPLCLTKH+4,X'20'    CLIENT WAS PREVIOUSLY VALIDATED?             
         BZ    VK00                NO - GO VALIDATE                             
         TM    SPLPRDKH+4,X'20'    PRODUCT WAS PREVIOUSLY VALIDATED?            
         BZ    VK00                NO - GO VALIDATE                             
         TM    SPLESTKH+4,X'20'    ESTIMATE WAS PREVIOUSLY VALIDATED?           
         BNZ   VK20                YES - WE GO THROUGH VK FOR VALREC!           
*                                                                               
VK00     MVI   USEIONUM,2          USE AIO2 FOR VMED,CLT,VPRD,VEST              
         LA    R2,SPLMEDKH         MEDIA HEADER                                 
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK05                YES                                          
         GOTO1 VALIMED             VALIDATE THE MEDIA                           
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   SPLMEDN,MEDNM       DISPLAY MEDIA NAME                           
         OI    SPLMEDNH+6,X'80'    TRANSMIT                                     
*                                                                               
VK05     LA    R2,SPLCLTKH         CLIENT HEADER                                
         GOTO1 VALICLT             VALIDATE THE CLIENT                          
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   SPLCLTN,CLTNM       DISPLAY CLIENT NAME                          
         OI    SPLCLTNH+6,X'80'    TRANSMIT                                     
*                                                                               
VK10     CLI   ACTEQU,ACTLIST      ACTION LIST?                                 
         BNE   VK11                NO - PRODUCT MUST BE VALIDATED               
         MVC   QPRD,SPACES         INIT QPRD TO SPACES                          
         CLI   SLSPRDKH+5,0        ANY INPUT                                    
         BE    VK15                NO - LEAVE QPRD AS SPACES                    
         CLC   SLSPRDK,=C'ALL'     ALL PRODUCTS?                                
         BE    VK15                YES - LEAVE QPRD AS SPACES                   
         OC    QPRD,SLSPRDK        MOVE PRODUCT TO SPACE PADDED QPRD            
         B     VK15                SET PRDOUCT AS VALIDATED                     
*                                                                               
VK11     LA    R2,SPLPRDKH         PRODUCT HEADER                               
         GOTO1 VALIPRD             VALIDATE THE PRODUCT                         
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   SPLPRDN,PRDNM       DISPLAY PRODUCT NAME                         
         OI    SPLPRDNH+6,X'80'    TRANSMIT                                     
*                                                                               
VK15     CLI   ACTEQU,ACTLIST      ACTION LIST?                                 
         BNE   VK16                NO - ESTIMATE MUST BE VALIDATED              
         LA    R2,SLSESTKH         ESTIMATE HEADER FOR LIST SCREEN              
         MVI   BEST,0              INIT ESTIMATE TO 0                           
         CLI   5(R2),0             ANY ESTIMATE INPUT?                          
         BE    VK20                NO - LEAVE ESTIMATE AS 0                     
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    ERRINV              NO - ERROR!                                  
         LLC   RE,5(R2)            ESTIMATE INPUT LENGTH                        
         BCTR  RE,0                -1 FOR EXECUTED PACK                         
         EX    RE,*+8              EXECUTE                                      
         B     *+10                BRANCH OVER PACK INSTRUCTION                 
         PACK  DUB,8(0,R2)         PACK THE ESTIMATE                            
         CVB   RE,DUB              CONVERT TO BINARY                            
         CHI   RE,1                LESS THEN 1?                                 
         BL    ERRINV              YES - ERROR                                  
         CHI   RE,255              GREATER THEN 255?                            
         BH    ERRINV              YES - ERROR                                  
         STC   RE,BEST             STORE START AT ESTIMATE IN BEST              
         B     VK20                DON'T READ DAYPART REC FOR LIST!             
*                                                                               
VK16     LA    R2,SPLESTKH         ESTIMATE HEADER                              
         GOTO1 VALIEST             VALIDATE THE ESTIMATE                        
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   SPLESTN,ESTNM       DISPLAY ESTIMATE NAME                        
         OI    SPLESTNH+6,X'80'    TRANSMIT                                     
*                                                                               
         L     R6,AIO              A(ESTIMATE RECORD)                           
         USING ESTHDRD,R6          ESTIMATE RECORD DSECT                        
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING DPTHDRD,R4          DAYPART DSECT                                
         MVI   DPTKTYPE,X'08'      DAYPART RECORD TYPE                          
         MVC   DPTKAGY,AGENCY      AGENCY CODE                                  
         MVC   DPTKMED,QMED        MEDIA CODE                                   
         MVC   DPTKMENU,EDAYMENU   DAYPART MENU FROM ESTIMATE RECORD            
         MVC   AIO,AIO2            PUT THE DAYPART RECORD IN AIO2               
         DROP  R4,R6                                                            
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND THE DAYPART RECORD?                    
         BNE   ERRNODPT            NO - ERROR                                   
*                                                                               
         GOTO1 GETREC              READ THE DAYPART RECORD INTO AIO2            
*                                                                               
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         BAS   RE,DISPDPT          DISPLAY THE DAYPART                          
*                                                                               
VK20     MVC   AIO,AIO1            USE AIO1 FOR SPLIT RECORD                    
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              BUILD THE SPLIT KEY                          
         USING SPLTRECD,R4         SPLIT RECORD DSECT                           
         MVC   SPLTKTYP,=XL2'0D40' MOVE RECORD TYPE X'0D40' TO KEY              
         MVC   SPLTKAM,BAGYMD      MOVE 1 BYTE BINARY AGENCY/MEDIA CODE         
         MVC   SPLTKCLT,BCLT       MOVE 2 BYTE BINARY CLIENT CODE               
         MVC   SPLTKPRD,QPRD       MOVES 3 BYTE EBCDIC PRODUCT CODE             
         MVC   SPLTKEST,BEST       MOVES 1 BYTE BINARY ESTIMATE CODE            
         B     XIT                 EXIT BACK TO GENCON                          
         DROP  R4                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       MVI   ELCODE,X'01'        REBUILD THE X'01' ELEMENT                    
         GOTO1 REMELEM             REMOVE THE X'01' ELEMENT                     
*                                                                               
         XC    ELEM,ELEM           BUILD NEW X'01' ELEMENT IN ELEM              
         LA    R6,ELEM             R6 = ELEM                                    
         USING SPLTEL01,R6         X'01' ACTIVITY ELEMENT DSECT                 
         MVI   SPLTEL01,X'01'      ACTIVITY ELEMENT CODE                        
         MVI   SPLTEL01+1,X'08'    ACTIVITY ELEMENT LENGTH                      
         GOTO1 DATCON,DMCB,(5,0),(3,SPLTACTD)  ACTIVITY DATE - TODAY            
         MVI   SPLTACT,C'A'        ACTION ADD                                   
         CLI   ACTEQU,ACTADD       IS THE ACTION ADD?                           
         BE    *+8                 YES                                          
         MVI   SPLTACT,C'C'        NO - ACTION MUST BE CHANGE                   
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM             ADD THE NEW X'01' ACTIVITY ELEMENT           
                                                                                
         MVI   ELCODE,X'02'        REBUILD THE X'02' ELEMENT                    
         GOTO1 REMELEM             REMOVE THE X'02' ELEMENT                     
*                                                                               
         XC    FULL,FULL           CLEAR FULL                                   
         USING SPLTEL02,R6         X'02' DAYPART ELEMENT DSECT                  
         XC    ELEM,ELEM           BUILD NEW X'02' ELEMENT IN ELEM              
         MVI   SPLTEL02,X'02'      ELEMENT CODE X'02'                           
         MVI   SPLTEL02+1,X'32'    ELEMENT LENGTH                               
         LA    R3,SPLDPT1H         DAYPART FIELD                                
         LA    R2,SPLFLD1H         PERCENTAGE FIELD                             
         LA    R7,SPLTDAYP         15 DAYPARTS - 3 BYTES EACH +3 SPARE          
         LA    R4,L'SPLTDAYP-3(R7) A(END OF DAYPARTS)                           
         LA    R5,35               MAX DAYPARTS ON SCREEN                       
         DROP  R6                                                               
*                                                                               
VR8      XR    R0,R0               CLEAR R4                                     
         ICM   R0,1,5(R2)          ANY PERCENTAGE INPUT?                        
         BZ    VR20                NO                                           
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,X'FF'          ANY ERROR?                                   
         BE    ERRVALUE            YES                                          
         L     R0,DMCB+4           R0 = VALUE                                   
         C     R0,=F'0'            ZERO VALUE?                                  
         BE    VR20                YES - IGNORE                                 
         BL    ERRLESS0            NEGATIVE VALUE IS INVALID                    
         C     R0,=F'10000'        GREATER THEN 100.00%?                        
         BH    ERR101              YES - ERROR                                  
*                                                                               
         MVC   0(1,R7),8(R3)       MOVE DPT CODE                                
         STCM  R0,3,1(R7)          STORE PERCENTAGE IN HEX                      
         L     R1,FULL             PERCENTAGE THUS FAR                          
         AR    R1,R0               ADD CURRENT PERCENTAGE                       
         ST    R1,FULL             NEW TOTAL PERCENTAGE                         
*                                                                               
         LA    R7,3(R7)            BUMP TO NEXT DAYPART                         
         CR    R7,R4               MORE THAN 15 PERCENTAGES?                    
         BNL   ERR2MANY            YES - ERROR                                  
VR20     LA    R3,LEN1(R3)         BUMP DAYPART FIELD                           
         LA    R2,LEN2(R2)         BUMP PERCENTAGE FIELD                        
         BCT   R5,VR8              PROCESS NEXT DAYPART                         
*                                                                               
VR30     CLC   FULL,=F'10000'      PERCENTAGE = 100?                            
         BE    VR35                YES                                          
         LA    R2,SPLFLD1H         CURSOR TO FIRST DAYPART FIELD                
         OI    6(R2),X'40'         POSITION CURSOR TO THIS FIELD                
         B     ERRNO100            TOTAL PERCENTAGE DOESNT = 100 ERROR          
*                                                                               
VR35     GOTO1 ADDELEM             ADD THE NEW X'02' ELEMENT                    
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       LA    R2,SPLFLD1H         PERCENTAGE FIELD                             
         LA    R3,35               USED FOR BCT LOOP                            
*                                                                               
DR10     XC    8(5,R2),8(R2)       CLEARS THE PERCENTAGE FIELDS                 
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,LEN2(R2)         BUMP TO NEXT PERCENTAGE FIELD                
         BCT   R3,DR10             LOOP BACK AND CLEAR THE NEXT ONE             
*                                                                               
         L     R6,AIO              A(SPLIT RECORD)                              
         MVI   ELCODE,X'02'        X'02' ELEMENT                                
         BAS   RE,GETEL            HAVE A X'02' ELEMENT?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - REQUIRED                                
         USING SPLTEL02,R6         ELEMENT X'02' DSECT                          
         LA    R2,SPLTDAYP         15 DAYPARTS - 3 BYTES EACH +3 SPARE          
         DROP  R6                                                               
*                                                                               
DR45     LA    R5,SPLDPT1H         DAYPART FIELD                                
         LA    R6,SPLFLD1H         PERCENTAGE FIELD                             
         LA    R3,35               MAX DAYPARTS ON SCREEN                       
*                                                                               
         CLI   0(R2),0             END OF DAYPARTS?                             
         BE    DR70                YES                                          
*                                                                               
DR50     CLC   8(1,R5),0(R2)       RECORD MATCHES SCREEN?                       
         BE    DR55                YES                                          
         LA    R5,LEN1(R5)         BUMP DAYPART FIELD                           
         LA    R6,LEN2(R6)         BUMP PERCENTAGE FIELD                        
         BCT   R3,DR50             LOOP BACK AND TEST REC VS. SCREEN            
         B     DR60                FELL THROUGH - DPT NOT FOUND!                
                                                                                
DR55     EDIT  (B2,1(R2)),(6,8(R6)),2                                           
*                                                                               
DR60     LA    R2,3(R2)            BUMP TO NEXT DAYPART                         
         B     DR45                AND DISPLAY THAT ONE                         
*                                                                               
DR70     XC    SPLMSG,SPLMSG       CLEAR THE LAST ACTIVITY                      
         OI    SPLMSGH+6,X'80'     TRANSMIT                                     
*                                                                               
         L     R6,AIO              A(SPILL RECORD)                              
         MVI   ELCODE,X'01'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL            HAVE AN ACTIVITY ELEMENT?                    
         BNE   DRX                 NO - EXIT                                    
*                                                                               
         LA    R2,SPLMSG           ACTIVITY MESSAGE                             
         MVC   0(13,R2),LASTACT    MOVE "LAST ACTIVITY" TO THE SCREEN           
         USING SPLTEL01,R6         ACTIVITY ELEMENT DSECT                       
         GOTO1 DATCON,DMCB,(3,SPLTACTD),(5,SPLMSG+14)                           
         MVC   24(3,R2),=C'ADD'    MOVE "ADD" TO THE SCREEN                     
         CLI   SPLTACT,C'A'        LAST ACTION AN ADD IN RECORD?                
         BE    *+10                YES                                          
         MVC   24(3,R2),=C'CHANGE' NO - MOVE "CHANGE" TO THE SCREEN             
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
*                                                                               
CLRSCRN  LA    R2,SPLDPT1H         FIRST FIELD HEADER                           
         XR    R1,R1               CLEAR R1                                     
*                                                                               
CS00     CLI   0(R2),0             END OF SCREEN?                               
         BER   RE                  YES                                          
         TM    1(R2),X'20'         IS THE FIELD IS PROTECTED?                   
         BNZ   CS10                YES - BUMP TO NEXT FIELD                     
         IC    R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SHI   R1,9                MINUS HEADER, AND 1 FOR EX                   
         EX    R1,*+8              EXECUTE                                      
         B     *+10                BRANCH OVER EXECUTED INSTRUCTION             
         XC    8(0,R2),8(R2)       BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CS10     IC    R1,0(R2)            LENGTH                                       
         AR    R2,R1               NEXT SCREEN FIELD                            
         B     CS00                YES - BUMP TO NEXT FIELD                     
*                                                                               
DISPDPT  NTR1                                                                   
         LA    R4,SPLDPT1H         FIRST DAYPART                                
         LA    R5,35               FOR BCT (MAX ON SCREEN)                      
         L     R6,AIO2             A(DAYPART RECORD)                            
         MVI   ELCODE,X'01'        ELEMENT CODE X'01'                           
         BAS   RE,GETEL            GET THE DAYPART ELEMENT                      
         LA    R6,2(R6)            BUMP PAST ELEMENT CODE AND LENGTH            
*                                                                               
DPT10    XC    8(5,R4),8(R4)       CLEAR DAYPART ON SCREEN                      
         OI    6(R4),X'80'         TRANSMIT                                     
         CLI   0(R6),0             END OF RECORD?                               
         BE    DPT20               YES - KEEP BUMPING & CLEARING SCREEN         
         CLI   0(R6),C'Z'          DAYPART Z?                                   
         BE    *+16                YES - SKIP                                   
         MVC   8(1,R4),0(R6)       MOVE DAYPART TO SCREEN                       
         MVC   10(3,R4),2(R6)      MOVE DAYPART TO SCREEN                       
         LA    R6,5(R6)            BUMP TO NEXT DAYPART IN ELEMENT              
DPT20    LA    R4,LEN1(R4)         NEXT PROTECTED HEADER                        
         BCT   R5,DPT10            LOOP BACK AND PROCESS NEXT DPT               
*                                                                               
DPTXIT   B     XIT                 EXIT                                         
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       L     R3,AIO                                                           
         USING SPLTRECD,R3                                                      
         MVC   BYTE,SPLTKAM        A/M                                          
         NI    BYTE,X'0F'          STRIP AGENCY AND ISOLATE MEDIA               
         LA    R5,MEDTAB           FIND MEDIA CODE USING MEDIA TABLE            
*                                                                               
DK10     CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                YES - INVALID MEDIA                          
         CLC   BYTE,1(R5)          MATCH ON MEDIA?                              
         BE    DK20                YES                                          
         LA    R5,MEDTABLQ(R5)     BUMP TO NEXT MEDIA ENTRY                     
         B     DK10                AND CHECK NEXT ENTRY                         
*                                                                               
DK20     MVC   SPLMEDK,0(R5)       MEDIA CODE                                   
         MVI   SPLMEDKH+5,1        INPUT LENGTH=1                               
         OI    SPLMEDKH+6,X'80'    TRANSMIT                                     
         NI    SPLMEDKH+4,X'FF'-X'20'                                           
*                                                                               
         GOTO1 CLUNPK,DMCB,SPLTKCLT,SPLCLTK                                     
         MVI   SPLCLTKH+5,3        CLIENT LENGTH=3                              
         OI    SPLCLTKH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVC   SPLPRDK,SPLTKPRD    PRODUCT CODE                                 
         MVI   SPLPRDKH+5,3        PRODUCT LENGTH=3                             
         OI    SPLPRDKH+6,X'80'    TRASNMIT                                     
*                                                                               
         EDIT  SPLTKEST,(3,SPLESTK),FILL=0                                      
         OI    SPLESTKH+4,X'08'    NUMERIC CODE                                 
         MVI   SPLESTKH+5,3        ESTIMATE LENGTH=3                            
         OI    SPLESTKH+6,X'80'    TRANSMIT                                     
*                                                                               
DKX      B     VK                  CALL VK TO READ THE DAYPART RECORD!          
         DROP  R3                                                               
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       MVI   NLISTS,13           LIST UP TO 13 RECORDS                        
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                NO                                           
*                                                                               
         LA    R4,KEY              R4 = KEY                                     
         USING SPLTRECD,R4         SPLIT RECORD DSECT                           
         MVC   SPLTKTYP,=XL2'0D40' RECORD TYPE X'0D40'                          
         MVC   SPLTKAM,BAGYMD      AGENCY/MEDIA CODE                            
         MVC   SPLTKCLT,BCLT       CLIENT CODE                                  
         MVC   SPLTKPRD,QPRD       PRODUCT START AT                             
         MVC   SPLTKEST,BEST       ESTIMATE START AT                            
         MVC   SAVEKEY,KEY         SAVE KEY FOR SECOND PASS INTO LR             
*                                                                               
LR10     GOTO1 HIGH                READ HIGH                                    
         B     LR21                GO TEST KEY                                  
*                                                                               
LR20     GOTO1 SEQ                 READ SEQ                                     
*                                                                               
LR21     CLC   KEY(5),SAVEKEY      SAME RECORD TYPE/MEDIA/CLIENT?               
         BNE   LRX                 NO MORE DAYPART SPLIT RECORD TO LIST         
         MVC   SVKEY(13),KEY       SAVE OFF THE KEY                             
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSPRDK,SPLTKPRD     MOVE PRODUCT CODE TO LIST SCREEN             
         EDIT  SPLTKEST,(3,LSESTK) MOVE ESTIMATE CODE TO LIST SCREEN            
*                                                                               
         L     R6,AIO              A(SPLIT RECORD)                              
         MVI   ELCODE,X'01'        ACTIVITY ELEMENT CODE                        
         BAS   RE,GETEL            HAVE ACTIVITY ELEMENT?                       
         BNE   LRPRD               NO                                           
         USING SPLTEL01,R6         SPLIT ELEMENT X'01' DSECT                    
         GOTO1 DATCON,DMCB,(3,SPLTACTD),(5,LSACTD)   ACTIVITY DATE              
         MVC   LSACT,=C'ADD'       MOVE ADD TO THE LIST SCREEN                  
         CLI   SPLTACT,C'A'        WAS THE LAST ACTIVITY AN ADD?                
         BE    LRPRD               YES                                          
         MVC   LSACT,=C'CHG'       NO - MOVE CHG TO THE LIST SCREEN             
         DROP  R6                                                               
*                                                                               
LRPRD    XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              PRODUCT RECORD KEY                           
         USING PRDHDRD,R6                                                       
         LA    R4,SVKEY            DAYPART SPLIT RECORD KEY                     
         MVC   PKEYAM,SPLTKAM      AGENCY/MEDIA CODE                            
         MVC   PKEYCLT,SPLTKCLT    CLIENT CODE                                  
         MVC   PKEYPRD,SPLTKPRD    PRODUCT CODE                                 
         CLC   SVPRDKEY,SPLTKPRD   SAME PRODUCT KEY AS LAST TIME?               
         BE    LRPRD10             YES - DON'T RE-READ PRD RECORD               
         MVC   SVPRDKEY,KEY        SAVE PRODUCT KEY                             
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     DOES THE PRODUCT RECORD EXIST?               
         BNE   ERRNOPRD            NO, ERROR                                    
*                                                                               
         GOTO1 GETREC              YES - GET THE RECORD                         
*                                                                               
         L     R6,AIO              A(PRODUCT RECORD)                            
         MVC   SVPNAME,PNAME       SAVE PRODUCT NAME                            
LRPRD10  MVC   LSPRDN,SVPNAME      PRODUCT NAME                                 
         DROP  R6                                                               
*                                                                               
LREST    MVC   LSESTN(17),=C'*** NOT FOUND ***'                                 
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              R6 = KEY                                     
         USING ESTHDRD,R6          ESTIMATE DSECT                               
         LA    R4,SVKEY            DAYPART SPLIT RECORD KEY                     
         MVC   EKEYAM,SPLTKAM      AGENCY/MEDIA CODE                            
         MVC   EKEYCLT,SPLTKCLT    CLIENT CODE                                  
         MVC   EKEYPRD,SPLTKPRD    PRODUCT CODE                                 
         MVC   EKEYEST,SPLTKEST    ESTIMATE NUMBER                              
         CLC   SVESTKEY,SPLTKPRD   SAME ESTIMATE KEY AS LAST TIME?              
         BE    LREST10             YES - DON'T RE-READ EST RECORD               
         MVC   SVESTKEY,KEY        SAVE ESTIMATE KEY                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     DOES THE ESTIMATE RECORD EXIST?              
         BNE   LR100               NO                                           
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R6,AIO              A(ESTIMATE RECORD)                           
         MVC   SVEDSEC,EDESC       SAVE THE ESTIMATE DESCRIPTION                
LREST10  MVC   LSESTN,SVEDSEC      MOVE ESTIMATE DESCRIPTION TO SCREEN          
         DROP  R6                                                               
*                                                                               
LR100    MVC   KEY(13),SVKEY       RESTORE SPLIT KEY                            
         GOTO1 HIGH                READ HIGH                                    
         GOTO1 GETREC              GET THE RECORD                               
         GOTO1 LISTMON             LIST THIS RECORD ON THE SCREEN               
         B     LR20                GO READ SEQ                                  
*                                                                               
LRX      B     XIT                 EXIT                                         
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE TO TRANSLATE A/M TO AGY                          *         
***********************************************************************         
MEDTAB   DC   CL1'T',XL1'01'       MEDIA T = X'01'                              
MEDTABLQ EQU  *-MEDTAB             MEDIA ENTRY LENGTH                           
         DC   CL1'R',XL1'02'       MEDIA R = X'02'                              
         DC   CL1'N',XL1'03'       MEDIA N = X'03'                              
         DC   CL1'X',XL1'04'       MEDIA X = X'04'                              
         DC   CL1'C',XL1'08'       MEDIA C = X'08'                              
         DC   X'FF'                X'FF' = END OF TABLE                         
         EJECT                                                                  
LASTACT  DC   CL13'LAST ACTIVITY'  "LAST ACTIVITY" CONSTANT                     
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID       INVALID                                      
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING       MISSING                                      
         B     VSFMERR                                                          
ERRNODPT MVC   ERRNUM,=AL2(859)    NO DAYPART ASSOCIATED WITH RECORD            
         B     SPERREX                                                          
ERR2MANY MVC   ERRNUM,=AL2(861)    THERE ARE MORE THAN 15 PERCENTAGES           
         B     SPERREX                                                          
ERRNO100 MVC   ERRNUM,=AL2(862)    TOTAL OF PERCENTAGES NOT 100                 
         B     SPERREX                                                          
ERRVALUE MVC   ERRNUM,=AL2(863)    INVALID INPUT PERCENTAGE                     
         B     SPERREX                                                          
ERRLESS0 MVC   ERRNUM,=AL2(864)    MAY NOT INPUT A NEGATIVE PERCENTAGE          
         B     SPERREX                                                          
ERR101   MVC   ERRNUM,=AL2(865)    PERCENTAGE MAY NOT SURPASS 100               
         B     SPERREX                                                          
ERRNOPRD MVC   ERRNUM,=AL2(41)     PRODUCT NOT ON FILE                          
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT   USE GETTXT INSTEAD OF GETMSG                 
         LA    RF,GETTXTCB         GETTXT CONTROL BLOCK                         
         USING GETTXTD,RF          GETTXT DSECT                                 
         MVC   GTMSGNO,ERRNUM      ERROR MESSAGE NUMBER                         
         MVI   GTMTYP,GTMERR       ERROR TYPE                                   
         MVI   GTMSYS,2            SPOT SYSTEM                                  
VSFMERR  MVC   AIO,AIO1            USE AIO1                                     
         GOTO1 ERREX               GO REPORT THE ERROR                          
         DROP  RF                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE  GETEL MACRO                                  
*                                                                               
LEN1     EQU   SPLDPT2H-SPLDPT1H   DAYPART LENGTH ON SCREEN                     
LEN2     EQU   SPLFLD2H-SPLFLD1H   PERCENTAGE LENGTH ON SCREEN                  
*                                                                               
         LTORG                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM5ED          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM5DD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPLT         SPLIT RECORD DSECT                           
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST          ESTIMATE RECORD DSECT                        
         EJECT                                                                  
DPTHDRD  DSECT                                                                  
       ++INCLUDE SPGENDAYPT        DAYPART RECORD DSECT                         
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD          PRODUCT RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
ERRNUM   DS    XL2                 ERROR NUMBER                                 
SAVEKEY  DS    CL13                SAVED KEY                                    
SVPRDKEY DS    CL13                SAVED PRODUCT                                
SVPNAME  DS    CL20                SAVE PRODUCT NAME                            
SVESTKEY DS    XL13                SAVED PRD/EST                                
SVEDSEC  DS    CL20                ESTIMATE NAME                                
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
         DS    CL1                                                              
LSPRDK   DS    CL3                                                              
         DS    CL3                                                              
LSPRDN   DS    CL20                                                             
         DS    CL3                                                              
LSESTK   DS    CL3                                                              
         DS    CL3                                                              
LSESTN   DS    CL20                                                             
         DS    CL3                                                              
LSACTD   DS    CL8                                                              
         DS    CL1                                                              
LSACT    DS    CL3                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPSFM64   06/02/10'                                      
         END                                                                    
