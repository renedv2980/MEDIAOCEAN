*          DATA SET SPSFM57    AT LEVEL 054 AS OF 05/13/09                      
*PHASE T21757A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE        T21757 - NEW SID RECORD COMPETITION MAINTENANCE       *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS DISPLAY                                      *         
*                                                                     *         
*  INPUTS       SCREEN T21763 (MAINTENANCE)                           *         
*                                                                     *         
*  OUTPUTS      UPDATED SID COMPETITION RECORDS                       *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- GETEL REGISTER                                  *         
*               R7 -- 2ND BASE REGISTER                               *         
*               R8 -- SPOOL                                           *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- 1ST BASE REGISTER                               *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21757 - CLIENT MAINTENANCE'                                    
T21757   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NMAXSTA*NTBLLENQ,**1757**,R7,RR=R3,CLEAR=YES                     
*                                                                               
         LR    R4,RC               SAVE A(TEMP WORKING STORAGE)                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         ST    R4,ANWSTBL                                                       
*                                                                               
         CLI   PFKEY,12            RETURN?                                      
         BNE   MAIN00                                                           
         TM    BITFLAG1,BF1XFRCN   YES, CAME FROM GLOBBER?                      
         BZ    MAIN00                                                           
         NI    BITFLAG1,X'FF'-BF1XFRCN  YES                                     
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'SFM'                                                 
         MVC   GLVXTOSY,XFRFRSYS                                                
         MVC   GLVXTOPR,XFRFRPRG                                                
         OI    GLVXFLG1,GLV1SEPS+GLV1RETN                                       
         DROP  R1                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    XIT                                                              
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,24,GLVXCTL                               
         B     XIT                                                              
*                                                                               
MAIN00   GOTO1 =A(SETUP),RR=RELO                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
         NI    BITFLAG1,X'FF'-BF1KYCHG                                          
         CLI   XFRCALL,C'Y'        COMING FROM GLOBBER?                         
         BNE   VK05                                                             
         OI    BITFLAG1,BF1XFRCN   YES, TO REMEMBER IN NEXT TRANSACTION         
         L     RE,ASFMPARM         THESE VALUES WERE SAVED IN SFM00             
         MVC   XFRFRSYS,0(RE)                                                   
         MVC   XFRFRPRG,3(RE)                                                   
         GOTO1 =A(SETGLOB),RR=RELO                                              
         GOTOR CALCFLEN,DMCB,(L'CONREC,CONRECH)                                 
*                                                                               
VK05     LA    R2,COMMEDH          MEDIA                                        
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         GOTO1 VALIMED             RETURN VALUE IN "BAGYMD"                     
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
         LA    R2,COMBYRH          BUYER NAME                                   
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         BAS   RE,VALBYR           RETURN VALUE IN "BBYR"                       
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
         LA    R2,COMCAMH          CAMPAIGN                                     
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         GOTO1 =A(VALICAM),RR=RELO                                              
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
****  BKVALSRC APPARENTLY DOES NOT GET SET BY SPSFM00                           
         MVI   BKVALSRC,C'N'       IF US                                        
         CLI   SVAPROF+7,C'C'      CANADA?                                      
         BNE   *+8                  - NOPE                                      
         MVI   BKVALSRC,C'C'        - YUP                                       
****                            MHC  12/08/04                                   
         LA    R2,COMSTAH          STATION NAME                                 
         GOTOR CALCFLEN,DMCB,(L'COMSTA,COMSTAH)                                 
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         GOTO1 ANY                 REQUIRED FIELD                               
         CLI   COMSTA,C'0'         IS IT CABLE?                                 
         BL    VK07                 - NOPE                                      
         MVI   COMSTA+4,C'/'        - YUP, MOVE IN A SLASH (FOR C'T')           
VK07     GOTO1 VALISTA             RETURN VALUES IN "BMKT"                      
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
         L     R6,AIO              TAKE THE STATION BK TYPE                     
         USING STARECD,R6                                                       
         MVC   STABKTYP,SBKTYPE                                                 
         CLI   STABKTYP,C'A'              (ONLY ALLOW A-Z)                      
         BL    *+12                                                             
         CLI   STABKTYP,C'9'                                                    
         BNH   *+8                                                              
         MVI   STABKTYP,0                                                       
****   NEED TO CHECK OUT THE POTENTIAL ESTIMATE BOOKTYPE OVERRIDE               
         BRAS  RE,GETEST           TO SET UP POTENTIAL OVERRIDING BOOK          
*                                                                               
         CLI   ESTBKTYP,C'A'              (ONLY ALLOW A-Z)                      
         BL    VK07G                                                            
         CLI   ESTBKTYP,C'9'                                                    
         BH    VK07G                                                            
         MVC   STABKTYP,ESTBKTYP   OVERRIDE IT                                  
VK07G    DS    0H                                                               
****   NEED TO CHECK OUT THE POTENTIAL ESTIMATE BOOKTYPE OVERRIDE               
         DROP  R6                                                               
*                                                                               
         LA    R2,COMDAYH          DAY                                          
         GOTOR CALCFLEN,DMCB,(L'COMDAY,COMDAYH)                                 
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         BAS   RE,VALDAY           RETURN VALUES IN "DAY"                       
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
         LA    R2,COMTIMH          TIME                                         
         GOTOR CALCFLEN,DMCB,(L'COMTIM,COMTIMH)                                 
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         BAS   RE,VALTIM           RETURN VALUES IN "TIME"                      
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
         LA    R2,COMDLNH          DAYPART/SPOT LENGTH                          
         GOTOR CALCFLEN,DMCB,(L'COMDLN,COMDLNH)                                 
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    BITFLAG1,BF1KYCHG   NO, KEY WAS CHANGED                          
         GOTO1 =A(VALDLN),RR=RELO                                               
         OI    4(R2),X'20'         NOW VALIDATED                                
*                                                                               
*                                  RETRANSMIT SCREEN FIELDS                     
VK10     OI    COMMEDH+6,X'80'                                                  
         OI    COMBYRH+6,X'80'                                                  
         OI    COMCAMH+6,X'80'                                                  
         OI    COMSTAH+6,X'80'                                                  
         OI    COMDAYH+6,X'80'                                                  
         OI    COMTIMH+6,X'80'                                                  
         OI    COMDLNH+6,X'80'                                                  
         OI    COMDEMH+6,X'80'                                                  
         OI    COMWKYH+6,X'80'                                                  
         OI    COMUPGH+6,X'80'                                                  
*                                  BUILD COMPETE REC KEY FOR LATER USE          
         LA    R4,SVCMPKEY                                                      
         USING CMPRECD,R4                                                       
         MVI   CMPKTYP,CMPKTYPQ                                                 
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,BAGYMD       AGENCY/MEDIA CODE                            
         MVC   CMPKBYR,BBYR        BUYER CODE                                   
         MVC   CMPKCAM,BCAM        CAMPAIGN                                     
         MVC   CMPKMKT,BMKT        SPOTPAK MARKET NUMBER                        
         MVC   CMPKDAY,XFRMBDAY    DAY CODE                                     
         MVC   CMPKTIME,XFRMBTIM   START & END TIME                             
         DROP  R4                                                               
*                                                                               
*WHEN CONTROL RETURN TO GENCON, GENCON WILL DO A DMREAD W/ WHATEVER IS          
*IN "KEY" (APPARENTLY, KEY CONTAINS THE DAYPART RECORD KEY BECAUSE THAT         
*IS THE LAST DATAMGR CALL).                                                     
*SO, GENCON WON'T GIVE ANY RECORD NOT FOUND ERROR.                              
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALBYR                                        *         
***********************************************************************         
*                              INPUT: R2=A(FIELD HEADER OF BUYER FIELD)         
*                             OUTPUT: BUYER CODE IN "BBYR"                      
VALBYR   NTR1                                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         LA    R4,KEY              BUILD KEY OF BUYER RECORD                    
         USING BYRRECD,R4                                                       
         XC    BYRKEY,BYRKEY                                                    
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAGMD,BAGYMD                                                  
         MVC   BYRKBYR,COMBYR                                                   
         OC    BYRKBYR,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   ERRBYR                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING BYRRECD,R6                                                       
*                                                                               
         MVI   ELCODE,BYRELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ELEMENT                       
         USING BYREL,R6                                                         
         MVC   BBYR,BYRCODE                                                     
         MVC   BBYRMASK,BYRMASK                                                 
         DROP  R6                                                               
*                                                                               
VBYRX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALDAY                                        *         
***********************************************************************         
*                              INPUT: R2=A(FIELD HEADER OF DAY FIELD)           
*                             OUTPUT: DAY CODE IN "DAY"                         
VALDAY   NTR1                                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         BAS   RE,GETEST           GET ESTIMATE OUT OF WEEK START DAY           
*                                                                               
VDAY10   ZIC   R4,COMDAYH+5                                                     
         OC    COMDAY,SPACES                                                    
         GOTO1 DAYVAL,DMCB,((R4),COMDAY),DAY,WORK                               
         CLI   DAY,0                                                            
         BE    ERRDAY                                                           
*                                                                               
         ZIC   RE,WORK                                                          
         STC   RE,HALF+1                                                        
         NI    HALF+1,X'0F'                                                     
         SRL   RE,4                                                             
         STC   RE,HALF                                                          
         CLI   ESTOWSDY,0          TEST OUT-OF-WEEK ROTATIONS                   
         BNE   VDAY20                                                           
         CLC   HALF(1),HALF+1      NO-CHECK START DAY LE END DAY                
         BH    ERRDAY                                                           
         B     VDAYX                                                            
*                                                                               
VDAY20   CLI   COMDAYH+5,7         TEST SPECIAL 7-BYTE DAY EXPRESSION           
         BNE   VDAY30                                                           
         CLC   COMDAY,=C'MTWTFSS'                                               
         BE    VDAYX                                                            
         LA    R0,7                                                             
         LA    R1,COMDAY                                                        
         CLI   0(R1),C'.'                                                       
         BE    VDAYX                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VDAY30   CLC   HALF(1),HALF+1      NO-CHECK DAYS DON'T CROSS START DAY          
         BE    VDAYX                  OF WEEK                                   
         BH    VDAY40                                                           
         CLC   HALF(1),ESTOWSDY                                                 
         BNL   VDAYX                                                            
         CLC   HALF+1(1),ESTOWSDY                                               
         BNL   ERRDAY                                                           
         B     VDAYX                                                            
*                                                                               
VDAY40   CLC   HALF(1),ESTOWSDY                                                 
         BL    ERRDAY                                                           
         CLC   HALF+1(1),ESTOWSDY                                               
         BNL   ERRDAY                                                           
*                                                                               
VDAYX    CLC   XFRMDAY,COMDAY                                                   
         BNE   XIT                                                              
         MVC   XFRMBDAY,DAY                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       GETEST                                        *         
***********************************************************************         
*                              INPUT: BAGYMD,BCLT,QPRD,BEST                     
*                             OUTPUT: AIO = A(ESTIMATE RECORD)                  
*                                     OOW START DAY  IN "ESTOWSDY"              
*                                     DAYPART MENU#  IN "ESTDMENU"              
*                                     DEMO CODE LIST IN "ESTDMLST"              
*                                     BOOKTYPE       IN "ESTBKTYP"              
GETEST   NTR1                                                                   
*                                                                               
         LA    R4,KEY              BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R4                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   ERREST                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         MVC   ESTOWSDY,EOWSDAY    OUT OF WEEK START DAY                        
         CLI   ESTOWSDY,1          TEST MONDAY                                  
         BNE   *+8                                                              
         MVI   ESTOWSDY,0          YES-IGNORE                                   
         MVC   ESTDMENU,EDAYMENU   DAYPART MENU NUMBER                          
         MVC   ESTBKTYP,EBKTYPE    BOOKTYPE                                     
*&&DO                                                                           
         MVC   ESTDMLST(L'EDEMLST),EDEMLST      DEMO CODE LIST                  
*&&                                                                             
         DROP  R6                                                               
*                                                                               
GESTX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALTIM                                        *         
***********************************************************************         
*                              INPUT: R2=A(FIELD HEADER OF TIME FIELD)          
*                             OUTPUT: RETURN DAY CODE IN "TIME"                 
VALTIM   NTR1                                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         ZIC   R4,COMTIMH+5                                                     
         GOTO1 TIMVAL,DMCB,((R4),COMTIM),TIME                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERRTIM                                                           
*                                                                               
VTIMX    CLC   XFRMTIM,COMTIM                                                   
         BNE   XIT                                                              
         MVC   XFRMBTIM,TIME                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       INSDEMO                                       *         
***********************************************************************         
*                              INPUT: R2=A(FLD HDR OF DEMO)                     
*                             OUTPUT: DEMO CODE IN "LVDEM"                      
INSDEMO  NTR1                                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,QMED                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+8                 NO                                           
         MVI   DBSELMED,C'C'       CHANGE MEDIA TO CANADIEN                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOVAL-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(1,(R2)),(1,DUB),(C'S',DBLOCK),0                       
         CLI   0(R1),0                                                          
         BNE   ERRDEM1                                                          
*                                                                               
         CLC   SVDEM,DUB           COMPARE TO THE OLD VALUE                     
         BE    IDE20                                                            
         MVC   SVDEM,DUB                                                        
         OI    LCHG,LCDEM                                                       
*                                                                               
*&&DO                                                                           
         LA    R4,ESTDMLST                                                      
IDE10    OC    0(L'ESTDMLST,R4),0(R4)   END OF DEMO CODE LIST                   
         BZ    ERRDEM2             DEMO CODE NOT FOUND IN EST RECORD            
         CLC   SVDEM(3),0(R4)                                                   
         BE    IDE20                                                            
         LA    R4,L'ESTDMLST(R4)                                                
         B     IDE10                                                            
*&&                                                                             
*                                                                               
IDE20    MVC   SVPUT,SVDEM         DETERMINE PUT AND SHARE DEMO CODES           
         MVC   SVRTGSHR(3),SVDEM                                                
         MVC   SVRTGSHR+3(4),SVDEM                                              
         MVI   SVPUT+1,C'P'                                                     
         MVI   SVRTGSHR+4,C'S'                                                  
         CLI   SVDEM+1,C'I'                                                     
         BNE   *+12                                                             
         MVI   SVPUT+1,C'Q'                                                     
         MVI   SVRTGSHR+4,C'X'                                                  
*                                                                               
         OI    4(R2),X'20'         MARK VALIDATED                               
*                                                                               
IDEX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       INSWKY                                        *         
***********************************************************************         
*                              INPUT: R2=A(FLD HDR OF WEEKLY)                   
*                             OUTPUT: SET FLAGS IN "SVWKYIND"                   
INSWKY   NTR1                                                                   
         XC    HALF,HALF                                                        
         TM    SVWKYIND,SVONEBK        CLEAR 3 REPEATED BOOKS                   
         BZ    IWKY00                                                           
         XC    SVBKS+2(6),SVBKS+2                                               
         XC    SVBKTPS+1(3),SVBKTPS+1   SVBK BOOK TYPES                         
IWKY00   NI    SVWKYIND,X'FF'-SVONEBK                                           
*                                                                               
         CLI   COMWKYH+5,0         ANY WEEKLY INPUT?                            
         BE    IWKY20                                                           
*                                                                               
         CLC   =C'W=',COMWKY       TEST SINGLE WEEK NUMBER                      
         BNE   IWKY10                                                           
         CLI   COMWKYH+5,3         YES-VALIDATE THE WEEK NUMBER                 
         BNE   ERRINV                                                           
         OC    SVWKY,SVWKY                                                      
         BNZ   IWKYX                                                            
*                                                                               
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
*                                                                               
***      GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LNQ,GLVBUY1  1ST DATA ELEM         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LQ2,GLVBUY1  1ST DATA ELEM         
         CLI   8(R1),GLEGNF        NOT FOUND?                                   
         BE    IWKYX                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         LA    R2,BLOCK                                                         
         USING NUKEM1D,R2                                                       
         MVC   COMBK1,NUKM1BKS                                                  
         GOTOR CALCFLEN,DMCB,(L'COMBK1,COMBK1H)                                 
         NI    COMBK1H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK2,NUKM1BKS+1*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK2,COMBK2H)                                 
         NI    COMBK2H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK3,NUKM1BKS+2*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK3,COMBK3H)                                 
         NI    COMBK3H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK4,NUKM1BKS+3*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK4,COMBK4H)                                 
         NI    COMBK4H+4,X'FF'-X'20'                                            
         DROP  R2                                                               
*                                                                               
         CLI   COMWKY+2,C'1'                                                    
         BL    ERRINV                                                           
         CLI   COMWKY+2,C'4'                                                    
         BH    ERRINV                                                           
*                                                                               
         MVI   HALF,C'W'                                                        
         MVC   HALF+1(1),COMWKY+2                                               
         OC    SVWKY,SVWKY         TEST WEEKLY ALREADY SET                      
         BZ    IWKY20                                                           
         CLI   SVWKY,C'W'          YES-TEST ONE BOOK, 4 WEEKS                   
         B     IWKY20                                                           
*                                                                               
IWKY10   GOTO1 SCANNER,DMCB,COMWKYH,(1,ELEM),C',=,-'                            
         CLI   4(R1),1                                                          
         BNE   ERRINV                                                           
         LA    R4,ELEM             VALIDATE MMM/YY-W FORM                       
         CLI   1(R4),1                                                          
         BNE   ERRINV                                                           
         CLI   22(R4),C'W'                                                      
         BNE   ERRINV                                                           
         GOTO1 DATVAL,(R1),(2,12(R4)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRINV                                                           
         OI    SVWKYIND,SVONEBK    INDICATE SINGLE BOOK, 4 WEEKS                
         GOTO1 DATCON,(R1),(0,WORK),(3,WORK+6)                                  
         MVC   HALF,WORK+6                                                      
         LA    R0,4                ALL 4 BOOKS THE SAME                         
         LA    R1,SVBKS                                                         
         MVC   0(2,R1),HALF                                                     
         LA    R1,2(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
IWKY20   CLC   HALF,SVWKY          TEST WEEKLY OPTION CHANGED                   
         BE    IWKYX                                                            
         MVC   SVWKY,HALF          YES-SAVE NEW WEEKLY OPTION                   
         OI    LCHG,LCBKS              AND INDICATE CHANGE                      
         OC    SVWKY,SVWKY                                                      
         BNZ   IWKYX                                                            
*                                                                               
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
*                                                                               
***      GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LNQ,GLVBUY1  1ST DATA ELEM         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LQ2,GLVBUY1  1ST DATA ELEM         
         CLI   8(R1),GLEGNF        NOT FOUND?                                   
         BE    IWKYX                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         LA    R2,BLOCK                                                         
         USING NUKEM1D,R2                                                       
         MVC   COMBK1,NUKM1BKS                                                  
         GOTOR CALCFLEN,DMCB,(L'COMBK1,COMBK1H)                                 
         NI    COMBK1H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK2,NUKM1BKS+1*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK2,COMBK2H)                                 
         NI    COMBK2H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK3,NUKM1BKS+2*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK3,COMBK3H)                                 
         NI    COMBK3H+4,X'FF'-X'20'                                            
*                                                                               
         MVC   COMBK4,NUKM1BKS+3*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK4,COMBK4H)                                 
         NI    COMBK4H+4,X'FF'-X'20'                                            
         DROP  R2                                                               
*                                                                               
IWKYX    OI    COMWKYH+4,X'20'     MARK VALIDATED                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0H                                                               
         TM    BITFLAG1,BF1KYCHG   IF THE KEY CHANGED                           
         BNZ   *+12                THEN LOOK UP AGAIN                           
         CLI   TWASTA,X'FF'        1ST REPORT?                                  
         BNE   DR10                                                             
         MVI   STANDIS,0                                                        
         BAS   RE,FSTRPT                                                        
         MVI   TWASTA,0                                                         
         B     DR50                                                             
*                                                                               
DR10     MVI   LCHG,0                                                           
*                                                                               
         LA    R2,COMDEMH          DEMO                                         
         GOTOR CALCFLEN,DMCB,(L'COMDEM,COMDEMH)                                 
         BAS   RE,INSDEMO                                                       
*                                                                               
         LA    R2,COMWKYH          WEEKLY                                       
         GOTOR CALCFLEN,DMCB,(L'COMWKY,COMWKYH)                                 
         BAS   RE,INSWKY                                                        
*                                                                               
         BAS   RE,INSBKS           BOOKS                                        
         TM    LCHG,LCBKS                                                       
         BZ    *+8                                                              
         BAS   RE,DISBKS                                                        
*                                                                               
         TM    LCHG,LCDEM+LCBKS                                                 
         BZ    DR18                                                             
         GOTO1 =A(GETPUTS),RR=RELO                                              
         GOTO1 =A(GETDEMS),RR=RELO                                              
*                                                                               
DR15     TM    LCHG,LCDEM          IF DEMO CHG, GET ALL PJ VALUES AGAIN         
         BZ    DR18                                                             
         MVI   STANDIS,0           STATION # ON DISPLAY ON 1ST LINE             
         BAS   RE,GETPJDEM                                                      
         BAS   RE,GETPJPUT                                                      
         GOTO1 =A(GETPJDOV),RR=RELO                                             
         GOTO1 =A(GETPJCUM),RR=RELO                                             
         B     DR20                                                             
*                                                                               
DR18     LA    R2,COMPPTH          PROJECTED PUT                                
         BAS   RE,INSPPT           INSPECT PROJECTION PUT                       
*                                                                               
         BRAS  RE,INSPGS           INSPECT ALL PROGRAM NAMES                    
*                                                                               
         BAS   RE,INSRSS           INSPECT ALL RATING AND SHARES                
*                                                                               
         TM    LCHG,LCPPT+LCPGS+LCRAT+LCSHR                                     
         BZ    DR20                                                             
         GOTO1 =A(WPJDEMS),RR=RELO  WRITE PJ DEMOS TO COMPETITION REC           
*                                                                               
         TM    LCHG,LCRAT+LCSHR                                                 
         BZ    DR19                                                             
         GOTO1 =A(GETPJCUM),RR=RELO                                             
*                                                                               
DR19     GOTO1 =A(CHNGNWS),RR=RELO  CHANGE THE NWS RECORDS                      
         CLI   ACTELOPT,C'N'       ARE WE WRITING?                              
         BE    ERR21P               - NOPE, ERROR, > 21 STATIONS                
*                                                                               
DR20     CLI   LCHG,0                                                           
         BNE   DR30                                                             
*                                                                               
         ZIC   RE,STANDIS                                                       
         AHI   RE,7                                                             
         STC   RE,STANDIS                                                       
*                                                                               
DR30     BRAS  RE,DISDEMS                                                       
         BAS   RE,DISPJDEM                                                      
         BAS   RE,DISSRC                                                        
*                                                                               
DR50     TM    SVFLAG,SVF2STA      DO WE SHOW THE SPECIAL MESSAGE?              
         BZ    DRX                                                              
         LA    R2,COMDAYH                                                       
         MVC   ERRNUM,=AL2(664)                                                 
         MVI   BLOCK,L'TOOMNYST+1               L'LENGTH+L'MESSAGE              
         MVC   BLOCK+1(L'TOOMNYST),TOOMNYST     MESSAGE                         
         MVI   BLOCK+1+L'TOOMNYST,0             TERMINATING 0                   
*                                                                               
         OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,23                                                        
         MVC   GTMSGNO,=AL2(62)                                                 
         MVI   GTMTYP,C'I'         INFO MESSAGE                                 
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         GOTO1 ERREX                                                            
*                                                                               
DRX      B     XIT                                                              
*                                                                               
TOOMNYST DC    C'* ONLY FIRST 21 STATIONS SHOWN *'                              
***********************************************************************         
*                       INSPPT                                        *         
***********************************************************************         
*                             INPUT: R2 - FIELD HEADER                          
INSPPT   NTR1                                                                   
         TM    COMPPTH+4,X'20'                                                  
         BO    IPPTX                                                            
*                                                                               
IPPT00   CLI   COMPPTH+5,0                                                      
         BNE   IPPT10                                                           
         CLI   CLTBWPRO+15,C'Y'    MISSING-TEST SUPPRESS PUTS                   
         BE    IPPTX               YES-OK                                       
         B     ERRMIS              NO-USER MUST ENTER SOMETHING                 
*                                                                               
IPPT10   CLI   CLTBWPRO+15,C'Y'    PRESENT-TEST SUPPRESS PUTS                   
         BE    ERRINV              YES-INVALID                                  
*                                                                               
         MVC   LDEMLST,SVPUT       GET THE ORIGINAL PJ PUT                      
         BRAS  RE,SDEMUP                                                        
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         ZIC   RF,COMPPTH+5                                                     
         ST    RF,DMCB+4                                                        
         LA    R1,COMPPT                                                        
*                                                                               
IPPT20   CLI   0(R1),C'*'          REMOVE THE * IF ANY                          
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,IPPT20                                                        
         B     IPPT30                                                           
*                                                                               
         CLC   SVPJPUT,LDEMVAL     COMPARE TO THE ORIGINAL PJ PUT               
         BE    *+14                                                             
         OI    LCHG,LCPPT          INDICATE PUT CHANGE                          
         MVC   SVPJPUT,LDEMVAL                                                  
         B     IPPT40                                                           
*                                                                               
IPPT30   GOTO1 CASHVAL,DMCB,(1,COMPPT)   VALIDATE PUT VALUE                     
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         CLC   SVPJPUT+1(3),DMCB+5   TEST CHANGE                                
         BE    IPPTX                                                            
         MVC   SVPJPUT,DMCB+4      YES                                          
         OI    SVPJPUT,X'80'       OVERRIDE BIT                                 
         OI    LCHG,LCPPT          INDICATE PUT CHANGE                          
*                                                                               
IPPT40   MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVC   FULL,SVPJPUT                                                     
         LA    RF,FULL                                                          
         NI    0(RF),255-X'80'                                                  
         LA    R1,FULL                                                          
         ST    R1,EBAIN                                                         
         MVI   EBLOUT,L'COMPPT                                                  
         LA    R1,COMPPT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         TM    SVPJPUT,X'80'                                                    
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         CLC   FULL,=F'1000'                                                    
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 EDITOR,DMCB,EBLOCK                                               
         OI    COMPPTH+6,X'80'                                                  
         OI    COMPPTH+4,X'20'                                                  
*                                                                               
IPPTX    B     XIT                                                              
***********************************************************************         
*                       INSRSS                                        *         
***********************************************************************         
INSRSS   NTR1                                                                   
*                                                                               
         ZIC   R6,NOSTA            # STATIONS ON THIS SCREEN                    
         LA    R2,COMRS1H                                                       
         LA    R3,SVPROJ           SAVED PJ DEMO VALUES LIST                    
         LA    R4,OLDPROJ          ORIGINAL PJ DEMO VALUES LIST                 
         ZIC   RE,STANDIS                                                       
         MHI   RE,8                                                             
         AR    R3,RE               TARGET DEMO VALUES                           
         AR    R4,RE               TARGET ORIGINAL DEMO VALUES                  
*                                                                               
IRSS10   TM    4(R2),X'20'         ANY CHANGE?                                  
         BO    IRSS90              NO - SKIP                                    
*                                                                               
         NI    LCHG,X'FF'-LCRAT-LCSHR                                           
*                                                                               
*                                  GET NEW RATING/SHARE                         
         ZIC   RE,5(R2)            RATING & SHARE INPUT LENGTH                  
         LR    RF,RE                                                            
         LA    R1,8(R2)                                                         
         STCM  R1,15,WORK+1        A(RATING INPUT FIELD)                        
*                                                                               
         CLI   0(R1),C'/'          SEARCH FOR /                                 
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
*                                                                               
         SR    RF,RE               RATING INPUT LENGTH                          
         STC   RF,WORK                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         STCM  R1,15,WORK+6        A(SHARE INPUT FIELD)                         
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,WORK+5           SHARE INPUT LENGTH                           
*                                                                               
*                                                                               
         XC    DUB,DUB                                                          
         ZICM  RF,WORK,1           RATING INPUT LENGTH                          
         BZ    ERRINV                                                           
         ST    RF,DMCB+4                                                        
         ICM   R1,15,WORK+1        A(RATING INPUT FIELD)                        
*                                                                               
IRSS20   CLI   0(R1),C'*'          REMOVE THE * IF ANY                          
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,IRSS20                                                        
         B     IRSS30                                                           
*                                                                               
         CLC   0(4,R3),0(R4)       COMPARE TO THE ORIGINAL RATING               
         BE    *+14                                                             
         OI    LCHG,LCRAT          INDICATE RATING CHANGE                       
         MVC   DUB(4),0(R4)                                                     
         B     IRSS40                                                           
*                                                                               
IRSS30   ICM   R5,15,WORK+1                                                     
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS30E                                                          
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS30E             IT IS IMPRESSION                             
         GOTO1 CASHVAL,DMCB,(2,(R5))                                            
         B     IRSS30G                                                          
***  2 DECIMAL                                                                  
IRSS30E  GOTO1 CASHVAL,DMCB,(1,(R5))          VALIDATE PUT VALUE                
IRSS30G  CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         CLC   1(3,R3),DMCB+5      TEST CHANGE                                  
         BE    IRSS40                                                           
         MVC   DUB(4),DMCB+4       YES                                          
         OI    DUB,X'80'           OVERRIDE BIT                                 
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS30T                                                          
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS30T             IT IS IMPRESSION                             
         OI    DUB,X'40'           NEED 2 DECIMAL BIT                           
***  2 DECIMAL                                                                  
IRSS30T  OI    LCHG,LCRAT          INDICATE RATING CHANGE                       
*                                                                               
IRSS40   CLI   CLTBWPRO+15,C'Y'    TEST SHARES ARE SUPPRESSED                   
         BE    IRSS70                                                           
         ZICM  RF,WORK+5,1         SHARE INPUT LENGTH                           
         BZ    ERRINV                                                           
         ST    RF,DMCB+4                                                        
         ICM   R1,15,WORK+6        A(SHARE INPUT FIELD)                         
*                                                                               
IRSS50   CLI   0(R1),C'*'          REMOVE THE * IF ANY                          
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,IRSS50                                                        
         B     IRSS60                                                           
*                                                                               
         OI    LCHG,LCSHR          INDICATE SHARE CHANGE                        
         MVC   DUB+4(4),4(R4)                                                   
         B     IRSS70                                                           
*                                                                               
IRSS60   ICM   R5,15,WORK+6                                                     
         GOTO1 CASHVAL,DMCB,(1,(R5))          VALIDATE SHARE VALUE              
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         CLC   5(3,R3),DMCB+5      TEST CHANGE                                  
         BE    IRSS70                                                           
         MVC   DUB+4(4),DMCB+4     YES                                          
         OI    DUB+4,X'80'         OVERRIDE BIT                                 
         OI    LCHG,LCSHR          INDICATE SHARE CHANGE                        
*                                                                               
IRSS70   TM    LCHG,LCSHR          SHARE CHANGE?                                
         BZ    IRSS80                                                           
*                                                                               
         MVC   4(4,R3),DUB+4       SAVE THE CHANGED SHARE                       
         B     IRSS100             RE-CALCULATE RATING                          
*                                                                               
IRSS80   TM    LCHG,LCPPT          RE-CALCULATE RATING                          
         BZ    IRSS85                                                           
*                                                                               
         L     RE,SVPJPUT          PUT                                          
         SLL   RE,1                ELIMINATE OVERRIDE BIT                       
         SRL   RE,1                                                             
         SR    R1,R1               PREPARE FOR DIVIDE                           
         L     R0,4(R3)            SHARE                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         MR    R0,RE                                                            
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS80E              - NO                                        
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS80E             IT IS IMPRESSION                             
         D     R0,=F'100'                                                       
         B     IRSS80G                                                          
***  2 DECIMAL                                                                  
IRSS80E  D     R0,=F'1000'                                                      
IRSS80G  LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R3)                                                         
         GOTOR TONWSTBL,DMCB,0                                                  
         CLC   0(4,R3),0(R4)       COMPARE THE ORIGINAL DEMO RATING             
         BE    *+8                                                              
         OI    0(R3),X'80'         RATING OVERRIDE BIT                          
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS85               - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS85              IT IS IMPRESSION                             
         OI    0(R3),DMODEM2D      TURN ON 2 DECIMAL BIT                        
***  2 DECIMAL                                                                  
*                                                                               
IRSS85   TM    LCHG,LCRAT                                                       
         BZ    IRSS110                                                          
*                                                                               
         MVC   0(4,R3),DUB         SAVE THE CHANGED RATING                      
*                                                                               
         LA    R1,SVPROJ           CHANGING THE RATING FOR THE SELECTED         
         CR    R1,R3                  RECORD?                                   
         BNE   *+10                                                             
         MVC   SVBWDRTG,DUB        YES, CHANGED THE SAVED VALUE TOO             
*                                                                               
         SR    R1,R1               RE-CALCULATE SHARE                           
         L     R0,DUB              RATING                                       
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
***  2 DECIMAL                                                                  
         TM    DUB,X'40'           WE DOING 2 DECIMAL?                          
         BZ    IRSS85C              - NOPE, NORMAL                              
         SLL   R0,1                ELIMINATE 2 DECIMAL BIT                      
         SRL   R0,1                SVPROJ STILL HAS THE BIT ON                  
***  2 DECIMAL                                                                  
IRSS85C  SRDL  R0,32                                                            
***  2 DECIMAL                                                                  
         TM    DUB,X'40'           WE DOING 2 DECIMAL?                          
         BZ    IRSS85E              - NOPE, NORMAL                              
         M     R0,=F'100'                                                       
         B     IRSS85G                                                          
***  2 DECIMAL                                                                  
*                                                                               
IRSS85E  M     R0,=F'1000'                                                      
IRSS85G  L     RE,SVPJPUT                                                       
         SLL   RE,1                                                             
         SRL   RE,1                                                             
*                                                                               
         LTR   RE,RE                                                            
         BNZ   *+10                                                             
         XR    R1,R1                                                            
         B     IRSS87                                                           
*                                                                               
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
IRSS87   ST    R1,4(R3)                                                         
*                                                                               
         GOTOR TONWSTBL,DMCB,0                                                  
         CLC   4(4,R3),4(R4)       COMPARE TO THE ORIGINAL VALUE                
         BE    *+8                                                              
         OI    4(R3),X'80'         OVERRIDE BIT                                 
         B     IRSS110                                                          
*                                                                               
IRSS90   TM    LCHG,LCPPT                                                       
         BZ    IRSS110                                                          
*                                                                               
IRSS100  L     RE,SVPJPUT          PUT                                          
         SLL   RE,1                ELIMINATE OVERRIDE BIT                       
         SRL   RE,1                                                             
         SR    R1,R1               PREPARE FOR DIVIDE                           
         L     R0,4(R3)            SHARE                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         MR    R0,RE                                                            
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS100E             - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS100E            IT IS IMPRESSION                             
         D     R0,=F'100'                                                       
         B     IRSS100G                                                         
***  2 DECIMAL                                                                  
IRSS100E D     R0,=F'1000'                                                      
IRSS100G LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R3)                                                         
         GOTOR TONWSTBL,DMCB,0                                                  
         CLC   0(4,R3),0(R4)       COMPARE THE ORIGINAL DEMO RATING             
         BE    *+8                                                              
         OI    0(R3),X'80'         RATING OVERRIDE BIT                          
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS110              - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS110             IT IS IMPRESSION                             
         OI    0(R3),DMODEM2D      TURN ON 2 DECIMAL BIT                        
***  2 DECIMAL                                                                  
*                                                                               
IRSS110  LA    R2,COMRS2H-COMRS1H(R2)                                           
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,IRSS10                                                        
*                                                                               
*                                  RE-CALCULATE ALL RATING IF PUT CHG           
         TM    LCHG,LCPPT                                                       
         BZ    IRSSX                                                            
*                                                                               
         ZIC   R6,SVSTANO          # STATIONS                                   
         LA    R3,SVPROJ           SAVED PJ DEMO VALUES LIST                    
         LA    R4,OLDPROJ          ORIGINAL PJ DEMO VALUES LIST                 
*                                                                               
IRSS120  L     RE,SVPJPUT          PUT                                          
         SLL   RE,1                ELIMINATE OVERRIDE BIT                       
         SRL   RE,1                                                             
         SR    R1,R1               PREPARE FOR DIVIDE                           
         L     R0,4(R3)            SHARE                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         MR    R0,RE                                                            
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS120E             - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS120E            IT IS IMPRESSION                             
         D     R0,=F'100'                                                       
         B     IRSS120G                                                         
***  2 DECIMAL                                                                  
IRSS120E D     R0,=F'1000'                                                      
IRSS120G LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R3)                                                         
         CLC   0(4,R3),0(R4)       COMPARE THE ORIGINAL DEMO RATING             
         BE    *+8                                                              
         OI    0(R3),X'80'         RATING OVERRIDE BIT                          
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    IRSS120K             - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   IRSS120K            IT IS IMPRESSION                             
         OI    0(R3),DMODEM2D      TURN ON 2 DECIMAL BIT                        
***  2 DECIMAL                                                                  
*                                                                               
IRSS120K LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,IRSS120                                                       
*                                                                               
         ZIC   R6,SVSTANO                                                       
         LA    RE,SVSTA                                                         
IRSS125  L     R1,ANWSTBL          MODIFIED ALL THE STATION RATINGS             
         USING NWSTBLD,R1                                                       
         LA    RF,NMAXSTA*NTBLLENQ(R1)                                          
*                                                                               
IRSS125A OC    0(NTBLLENQ,R1),0(R1)  ANYTHING HERE?                             
         BZ    IRSS125B                                                         
         CLC   NTBLQSTA,0(RE)      SAME STATION?                                
         BE    IRSS125B                                                         
         LA    R1,NTBLLENQ(R1)                                                  
         CR    R1,RF                                                            
         BL    IRSS125A                                                         
         DC    H'0'                                                             
*                                                                               
IRSS125B MVC   NTBLQSTA,0(RE)                                                   
         OI    NTBLFLAG,NTBLFRTG   RATING CHANGED                               
         LA    R1,NTBLLENQ(R1)                                                  
         LA    RE,L'SVSTA(RE)                                                   
         BCT   R6,IRSS125                                                       
*                                                                               
*                                                                               
IRSSX    B     XIT                                                              
***********************************************************************         
*                       FSTRPT                                        *         
***********************************************************************         
FSTRPT   NTR1                                                                   
*                                                                               
****  GONNA CHECK THE 00 PROFILE FOR 2 DECIMAL                                  
         XC    WORK,WORK           READ 00 PROFILE                              
         MVC   WORK+16(4),=C'S000'                                              
         MVC   WORK+20(2),AGENCY                                                
***  USING X'D0' FOR RETURNING DEFAULT PROFILE AND NOT USER ID                  
         GOTO1 GETPROF,DMCB,(X'D0',WORK+16),WORK,DATAMGR                        
         CLI   WORK+9,C'Y'                                                      
         BNE   *+8                                                              
         OI    BITFLAG1,BF1TWODC   2 DECIMAL OK!!                               
****  GONNA CHECK THE 00 PROFILE FOR 2 DECIMAL                                  
*                                                                               
         LA    R2,COMDEMH          DEMO                                         
         BAS   RE,INSDEMO                                                       
*                                                                               
         LA    R2,COMWKYH          WEEKLY                                       
         BAS   RE,INSWKY                                                        
*                                                                               
         OC    CMPUP,CMPUP         TEST FOR CAMPAIGN UPGRADE                    
         BZ    FR10                                                             
         TM    BITFLAG1,BF1XFRCN   COMING FROM NWS14?                           
         BNZ   FR10                YES, USE WHAT WE GET FROM GLOBBER            
         MVC   SVUFILE,CMPUF                                                    
         MVC   SVUGRD,CMPUP                                                     
         MVC   SVUFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   FR08                                                             
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUFRBT,CMPFBTP     SVUFRBT BOOKTYPE                             
FR08     MVC   SVUFBL,CMPFBLST                                                  
         MVC   SVUINP,CMPUPIN                                                   
         MVC   SVUPUT,CMPUPUT                                                   
         MVC   SVUSHR,CMPUSHR                                                   
*                                                                               
FR10     BAS   RE,INSBKS                                                        
         BAS   RE,DISBKS                                                        
*&&DO                                                                           
         GOTO1 =A(GETPUTS),RR=RELO                                              
         BAS   RE,GETPJPUT                                                      
         BAS   RE,GETSTAS                                                       
         GOTO1 =A(GETDEMS),RR=RELO                                              
         BRAS  RE,DISDEMS                                                       
         BAS   RE,GETPJDEM                                                      
         GOTO1 =A(GETPJDOV),RR=RELO                                             
         GOTO1 =A(GETRATOV),RR=RELO                                             
         GOTO1 =A(GETPJCUM),RR=RELO                                             
         BAS   RE,DISPJDEM                                                      
         BAS   RE,DISSRC                                                        
*&&                                                                             
*                                                                               
         BAS   RE,GETPJPUT                                                      
         GOTO1 =A(GETPUTS),RR=RELO                                              
         BAS   RE,GETSTAS                                                       
         GOTO1 =A(GETDEMS),RR=RELO                                              
         BRAS  RE,DISDEMS                                                       
         BAS   RE,GETPJDEM                                                      
         GOTO1 =A(GETPJDOV),RR=RELO                                             
         GOTO1 =A(GETRATOV),RR=RELO                                             
         GOTO1 =A(GETPJCUM),RR=RELO                                             
         BAS   RE,DISPJDEM                                                      
         BAS   RE,DISSRC                                                        
*                                                                               
*                                                                               
FRX      B     XIT                                                              
***********************************************************************         
*                       INSBKS                                        *         
***********************************************************************         
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LCBKS IF BOOKS WERE CHANGED                         *         
***********************************************************************         
INSBKS   NTR1                                                                   
*                                                                               
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BO    IBKSX               YES-BOOKS ALREADY DETERMINED                 
         LA    R4,COMBK1H                                                       
         LR    R2,R4                                                            
         CLI   5(R2),0             1ST BOOK IS REQUIRED                         
         BE    ERRMIS                                                           
         LA    R5,SVBKS                                                         
         LA    R3,SVBKTPS          SVBKS BOOK TYPES                             
*                                                                               
IBKS10   LR    R1,R4               VALIDATE BOOK FIELD                          
         LR    R2,R4                                                            
         TM    4(R4),X'20'         PREVIOUSLY VALIDATED?                        
         BO    IBKS60              NEXT FIELD THEN                              
         CLI   5(R4),0                                                          
         BNE   IBKS20                                                           
         OC    0(2,R5),0(R5)       MISSING                                      
         BZ    IBKS60                                                           
         XC    0(2,R5),0(R5)                                                    
         OI    LCHG,LCBKS                                                       
         B     IBKS60                                                           
*                                                                               
IBKS20   MVI   WORK+16,0           CLEAR NWS BITS FOR OR-ING                    
         CLI   11(R1),C'/'         DO WE HAVE A SLASH?                          
         BNE   IBKS20E              - NOPE                                      
         LA    R1,14(R1)                                                        
         B     IBKS25                                                           
*                                                                               
IBKS20E  LA    R1,13(R1)           TEST FOR '-WEEK'                             
         CLI   0(R1),C'-'                                                       
         BE    IBKS40                                                           
*                                                                               
* LOOK FOR BOOK TYPES THAT ARE VALID FOR NWS                                    
*                                                                               
         LA    RE,0(R1)                                                         
         ST    RE,DMCB                                                          
         MVI   DMCB,C'C'                                                        
         LA    RE,WORK+16          NWS BITS SAVED HERE                          
         ST    RE,DMCB+4                                                        
         LA    RE,WORK+18          FOR 2 CHARACTER BOOK TYPES                   
         ST    RE,DMCB+8                                                        
         LR    R0,R1                                                            
         LA    R1,DMCB                                                          
         BRAS  RE,GTBKTYPE                                                      
         LR    R1,R0                                                            
         CLI   DMCB,X'FF'          ERROR RETURNING FROM GTBKTYPE?               
         BNE   IBKS30              NO                                           
*                                                                               
         LA    R1,1(R1)            MAYBE NEXT CHARACTER HAS IT?                 
*                                                                               
IBKS25   CLI   0(R1),C'-'                                                       
         BE    IBKS40                                                           
*                                                                               
         LA    RE,0(R1)                                                         
         ST    RE,DMCB                                                          
         MVI   DMCB,C'C'                                                        
         LA    RE,WORK+16          NWS BITS SAVED HERE                          
         ST    RE,DMCB+4                                                        
         LA    RE,WORK+18          FOR 2 CHARACTER BOOK TYPES                   
         ST    RE,DMCB+8                                                        
         LR    R0,R1                                                            
         LA    R1,DMCB                                                          
         BRAS  RE,GTBKTYPE                                                      
         LR    R1,R0                                                            
         CLI   DMCB,X'FF'          ERROR RETURNING FROM GTBKTYPE?               
         BE    IBKS50              NEITHER WEEKLY NOR VALID BOOK DATA           
*                                                                               
IBKS30   CLI   0(R1),C'+'          IGNORE LPM?                                  
         BE    IBKS50               - YUP                                       
***      CLI   1(R1),C' '                                                       
***      BH    *+12                                                             
***      MVI   0(R1),C' '                                                       
***      B     IBKS50                                                           
***      MVC   WORK+16(1),0(R1)                                                 
***      MVI   0(R1),C' '                                                       
         CLI   1(R1),C' '                                                       
         BNH   IBKS32                                                           
***      MVC   WORK+17(1),1(R1)                                                 
***      MVI   1(R1),C' '                                                       
IBKS32   OC    SVWKY,SVWKY         WE DOING WEEKLY?                             
         BZ    IBKS50               - NOPE                                      
         B     IBKS42               - YUP                                       
*                                                                               
IBKS40   OC    SVWKY,SVWKY         YES-TEST WEEKLY OPTION SET                   
         BZ    ERRBOOK             NO-ERROR                                     
IBKS42   CLC   1(1,R1),SVWKY+1     YES-TEST CORRECT WEEK                        
         BNE   ERRBOOK                                                          
         MVC   0(2,R1),SPACES                                                   
*                                                                               
IBKS50   LA    R6,8(R4)                                                         
         GOTO1 DATVAL,DMCB,(2,(R6)),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRBOOK             INVALID DATE                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
*                                                                               
         CLI   WORK+16,0           STANDARD BOOK ON INPUT?                      
         BNE   IBKS55                                                           
         CLI   STABKTYP,0          STATION HAS A BOOK TYPE?                     
         BE    IBKS56              NO, TRULY STANDARD, BUT MAYBE LPM            
         CLI   STABKTYP,C'H'       STATION BOOK TYPE HISPANIC?                  
         BNE   IBKS59              NO, CAN'T HAVE LPM THEN                      
         MVI   WORK+16,BTYHISPQ                                                 
*                                                                               
IBKS55   CLI   WORK+16,BTYHISPQ    HISPANIC BOOK ON INPUT?                      
         BNE   IBKS57              NOPE, WE HAVE AN ACTUAL BOOK TYPE            
*                                                                               
IBKS56   OC    SVMLPMSD,SVMLPMSD   ANY LPM START DATE?                          
         BZ    IBKS57                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVMLPMSD),(3,WORK+20)                             
         CLC   WORK+6(2),WORK+20   ON OR AFTER LPM START DATE?                  
         BNL   IBKS56H              - YES                                       
         CLI   6(R6),C'+'          UNDERSCORE?                                  
         BNE   IBKS57               - NAH                                       
         MVI   6(R6),C' '          TAKE OUT THE +                               
         OI    6(R4),X'80'         TRANSMIT                                     
         B     IBKS57              NOPE                                         
*                                                                               
IBKS56H  CLI   6(R6),C'+'          DO YOU REALLY NEED LPM?                      
         BE    IBKS59               - NOPE, WE DON'T!                           
*                                                                               
         CLI   WORK+16,BTYHISPQ    YES,  HISP -> HISP PEOPLE METER              
         BNE   *+12                                                             
         MVI   WORK+16,BTYHPEOQ                                                 
         B     IBKS57                                                           
         MVI   WORK+16,BTYPEOPQ          STANDARD -> PEOPLE METER               
*                                                                               
IBKS57   OC    WORK+6+1(1),WORK+16   OR IN THE NWS BITS FOR BOOK TYPE           
*                                                                               
IBKS59   CLC   0(2,R5),WORK+6      COMPARE TO OLD VALUE                         
         BNE   IBKS59E                                                          
         CLC   0(1,R3),WORK+18                                                  
         BE    IBKS60                                                           
IBKS59E  MVC   0(2,R5),WORK+6      SAVE BOOK YR/MN                              
         MVC   0(1,R3),WORK+18     BINARY BOOKTYPE                              
         OI    LCHG,LCBKS                                                       
*                                                                               
IBKS60   LA    R4,COMBK2H-COMBK1H(R4)   NEXT BOOK                               
         LA    R5,2(R5)                                                         
         LA    R3,1(R3)            NEXT BOOK TYPE                               
         LA    R0,COMPT4H                                                       
         CR    R4,R0               DO FOR ALL THE 4 BOOKS                       
         BL    IBKS10                                                           
*                                                                               
IBKSX    B     XIT                                                              
***********************************************************************         
*                       DISBKS                                        *         
***********************************************************************         
DISBKS   NTR1                                                                   
         LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R3,SVBKTPS          SVBKS BOOK TYPES                             
         LA    R5,COMBK1H                                                       
         LA    R6,C'1'                                                          
         MVI   WORK+2,1                                                         
*                                                                               
DBKS10   CLI   14(R5),C'+'         IGNORE LPM?                                  
         BE    *+10                 - YUP, DON'T CLEAR OUT BK ON SCREEN         
         XC    8(L'COMBK1,R5),8(R5)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DBKS20                                                           
         MVC   WORK(2),0(R4)                                                    
         NI    WORK+1,X'FF'-BTYBITSQ   REMOVE SPECIAL TYPE BITS                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,8(R5))                                   
*                                                                               
         TM    1(R4),BTYBITSQ      ANY BOOK TYPE?                               
         BZ    DBKS12                                                           
         GOTO1 =A(GTBKTYPE),DMCB,(C'B',1(R4)),14(R5),0(R3),RR=RELO              
         B     DBKS18                                                           
*                                                                               
DBKS12   CLI   STABKTYP,C'H'       ONLY HISPANIC OR NO BOOK TYPE CAN            
         BE    *+12                   HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   DBKS18                                                           
*                                                                               
         OC    SVMLPMSD,SVMLPMSD   ANY LPM START DATE?                          
         BZ    DBKS18              NONE                                         
         GOTO1 DATCON,DMCB,(2,SVMLPMSD),(3,FULL)                                
         CLC   0(2,R4),FULL        ON OR AFTER LPM START DATE?                  
         BL    DBKS18                                                           
         CLI   14(R5),C'+'         IGNORING LPM?                                
         BE    DBKS18               - YES WE ARE                                
*                                                                               
         CLI   STABKTYP,C'H'       STATION OR EST HAS HISPANIC BOOK?            
         BNE   *+12                                                             
         MVI   14(R5),C'I'         YES, SET ON HISPANIC PEOPLE METER            
         B     *+8                                                              
         MVI   14(R5),C'P'         YES, SET ON PEOPLE METER                     
*                                                                               
DBKS18   OC    SVWKY,SVWKY         WEEKLY?                                      
         BZ    DBKS20                                                           
*                                                                               
         MVC   11(3,R5),12(R5)     GET RID OF THE '/'                           
*                                                                               
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION                           
         BZ    DBKS20                                                           
         STC   R6,14(R5)                                                        
*                                                                               
         CLI   SVWKY,C'W'                                                       
         BNE   DBKS20                                                           
         MVC   14(1,R5),SVWKY+1    4 BOOKS, 1 WEEK                              
*                                                                               
DBKS20   OI    6(R5),X'80'         TRANSMIT BKS                                 
         OI    4(R5),X'20'         MARK VALIDATED                               
         LA    R4,2(R4)                                                         
         LA    R3,1(R3)            BUMPING THE SVBKTPS                          
         LA    R5,COMBK2H-COMBK1H(R5)                                           
         LA    R6,1(R6)                                                         
         BCT   R0,DBKS10                                                        
*                                                                               
DBKSX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       GETPJPUT                                      *         
***********************************************************************         
GETPJPUT NTR1                                                                   
*                                                                               
GETJ4    MVC   LDEMLST,SVPUT       CALL SPDEMUP FOR PROJECTED PUT               
         BRAS  RE,SDEMUP                                                        
         CLI   TWASTA,X'FF'        1ST REPORT?                                  
         BNE   GETJ6               NO, DISREGARD CHANGES TO KEY                 
         MVC   SVPJPUT,LDEMVAL     GET INITIAL VALUE                            
*                                                                               
GETJ6    LA    R1,SVPJPUT                                                       
         LHI   RF,10000                                                         
*                                                                               
GETJ8    ST    R1,EBAIN            FORMAT PROJECTED PUT                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         LA    RE,COMPPT                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'COMPPT                                                  
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLM   RF,7,1(R1)                                                       
         BH    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
GETJX    OI    COMPPTH+6,X'80'     TRANSMIT PJ PUT                              
         OI    COMPPTH+4,X'20'     MARK VALIDATED                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       GETPJDEM                                                
***********************************************************************         
GETPJDEM NTR1                                                                   
         CLI   TWASTA,X'FF'        1ST REPORT?                                  
         BNE   GPJDX               'SVPROJ' & 'SVPJPRGS' ARE STATIC             
*                                                                               
         OC    SVUGRD,SVUGRD       ANY UPGRADE?                                 
         BZ    GPJDX               NONE, EXIT LIKE WE DO IN SDEMUP              
*                                                                               
         MVI   SVRTGSHR,0          REMOVE OVERRIDE INDICATORS                   
         MVI   SVRTGSHR+3,0                                                     
*                                                                               
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,AIO3                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPLPM,SVMLPMSD                                                 
         MVC   SPUPMALF,SVMALPHA                                                
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
         MVC   SPUPMED,QMED                                                     
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,BMKT                                                     
         MVC   SPUPDAY,DAY                                                      
         MVC   SPUPTIM,TIME                                                     
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUFRBK                                                  
         MVC   SPUPFBKL,SVUFBL                                                  
         MVC   SPUPUDAY,SVUDAY                                                  
         MVC   SPUPUTIM,SVUTIM                                                  
*                                                                               
         CLI   QMED,C'C'          TEST CANADA                                   
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         MVC   SPUPTYPE(L'SVUGRD),SVUGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
****     TM    SPUPFBK+1,X'80'     OLYMPIC BOOK?                                
****     BZ    *+12                                                             
****     MVI   SPUPBTYP,C'O'       YES                                          
****     NI    SPUPFBK+1,X'FF'-X'80'                                            
         TM    SPUPFBK+1,BTYBITSQ  ANY SPECIAL BOOK?                            
         BZ    GPJD09              NO                                           
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPES?                       
         BNO   GPJD08R                                                          
         CLI   SVUFRBT,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,SVUFRBT                                                 
         B     GPJD08T              - YUP, ALREADY HAVE QBOOKTYP                
*                                                                               
GPJD08R  GOTOR =A(GTBKTYPE),DMCB,(C'B',SPUPFBK+1),SPUPBTYP,RR=RELO              
GPJD08T  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
GPJD09   CLI   SVUPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
         ZIC   R2,SVSTANO          R2 = NUMBER OF STATIONS                      
         LA    R5,SVPJPRGS         R5 = A(PROGRAM NAMES)                        
         LA    R3,SVSTA            R3 = A(STATION LIST)                         
         LA    R6,SVPROJ           R6 = A(DEMO VALUE AREA)                      
         LR    RE,R6               CLEAR DEMO VALUE AREA                        
         LA    RF,SVPROJL                                                       
         XCEFL                                                                  
         LR    RE,R5               CLEAR PROGRAM NAME AREA                      
         LA    RF,SVPJPRGL                                                      
         XCEFL                                                                  
         XC    SVPJCUM,SVPJCUM     CLEAR CUM SHARES                             
*                                                                               
GPJD10   DS    0H                                                               
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   0(R3),C'0'          IS IT A NUMBER?                              
         BL    GPJD10E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         XC    SPUPSYSC,SPUPSYSC   CLEAR THIS SO 2ND+ INTERATION WORKS!         
         MVC   SPUPSTA(3),5(R3)      MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,0(R3)                                                   
         B     GPJD10G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
GPJD10E  MVC   SPUPSTA,0(R3)       STATION                                      
GPJD10G  MVC   SPUPSPL,8(R3)       SPILL MARKET (IF ANY)                        
*                                                                               
         XC    SPUPPRG,SPUPPRG     CLEAR PREV PROGRAM NAME                      
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GPJD15               - NOPE                                      
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
***      MVC   SPUPMALF(3),=C'BAK'                                              
GPJD15   GOTO1 VSPDEMUP,DMCB,LDMUPBLK,SVRTGSHR,(R6)                             
*                                                                               
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
         MVC   0(L'L1PJPROG,R5),SPUPPRG    PROGRAM NAME                         
*                                                                               
         CLI   SVRTGSHR+3,OVERELEM NO-TEST SHARE OR PUT OVERRIDE                
         BE    GPJD20              YES-ADJUST THE RATING                        
         B     GPJD30                                                           
*                                                                               
GPJD20   SR    R1,R1               ** ADJUST THE RATING **                      
         L     R0,SVPJPUT                                                       
         SLL   R0,1                                                             
         SRDL  R0,32                                                            
         L     RE,4(R6)                                                         
         MR    R0,RE                                                            
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GPJD25E              - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GPJD25E             IT IS IMPRESSION                             
         D     R0,=F'100'                                                       
         B     GPJD25G                                                          
***  2 DECIMAL  ***                                                             
GPJD25E  D     R0,=F'1000'                                                      
GPJD25G  LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R6)                                                         
         OI    0(R6),X'80'         RATING OVERRIDE BIT                          
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GPJD30               - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GPJD30              IT IS IMPRESSION                             
         OI    0(R6),X'40'         TURN ON 2 DECIMAL BIT                        
***  2 DECIMAL  ***                                                             
*                                                                               
GPJD30   MVI   SVRTGSHR,0          REMOVE OVERRIDE INDICATORS                   
         MVI   SVRTGSHR+3,0                                                     
*        L     R1,SVPJCUM          ACCUMULATE PROJ CUM SHARE                    
*        SR    RE,RE                                                            
*        ICM   RE,7,5(R6)                                                       
*        AR    R1,RE                                                            
*        ST    R1,SVPJCUM                                                       
         LA    R5,L'SVPJPRGS(R5)   NEXT STATION                                 
         LA    R3,L'SVSTA(R3)                                                   
         LA    R6,8(R6)                                                         
         BCT   R2,GPJD10                                                        
*                                                                               
*        BRAS  RE,FMTPJCUM         FORMAT PROJ CUM SHARE                        
*                                                                               
*                                                                               
GPJDX    MVC   OLDPROJ(SVPROJL),SVPROJ  SAVE THE ORIGINAL DEMO VALUES           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPJDEM                                      *         
***********************************************************************         
DISPJDEM NTR1                                                                   
*                                  FORMAT STATIONS                              
         CLC   STANDIS,SVSTANO                                                  
         BL    *+8                                                              
         MVI   STANDIS,0                                                        
*                                                                               
         LA    R0,7                # OF STATION CAN BE DISPLAYED                
         ZIC   RE,STANDIS                                                       
         ZIC   RF,SVSTANO                                                       
         SR    RF,RE                                                            
         CR    RF,R0               FULL SCREEN?                                 
         BNL   *+6                 YES                                          
         LR    R0,RF               ACTUAL # STATION ON DISPLAY                  
         STC   R0,NOSTA                                                         
*                                                                               
         LA    R1,7                                                             
         SR    R1,R0               # OF NULL STATION LINES ON DISPLAY           
         LA    R0,7                # OF STATION CAN BE DISPLAYED                
*                                                                               
         ZIC   R3,STANDIS                                                       
         MHI   R3,L'SVPJPRGS                                                    
         LA    R3,SVPJPRGS(R3)      TARGET PROGRAM NAME                         
*                                                                               
         ZIC   R4,STANDIS                                                       
         MHI   R4,8                                                             
         LA    R4,SVPROJ(R4)        TARGET RATING/SHARE                         
*                                                                               
         LA    R5,COMPG1H                                                       
         LA    R2,COMRS1H                                                       
*                                                                               
DPJD10   CR    R0,R1               NULL LINE?                                   
         BH    DPJD20              NO                                           
*                                                                               
         XC    8(L'COMPG1,R5),8(R5)  CLEAR THE ENTIRE LINE                      
         XC    8(L'COMRS1,R2),8(R2)  CLEAR THE ENTIRE LINE                      
         B     DPJD30                                                           
*                                                                               
DPJD20   MVC   8(L'L1PROG1,R5),0(R3) PROGRAM NAMES                              
*                                                                               
*                                  FORMAT RATINGS AND SHARES                    
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         XC    8(L'COMRS1,R2),8(R2)                                             
         LA    R6,8(R2)            FOR FMTRS CALL, SET R4 & R6                  
         BRAS  RE,FMTRS            FORMAT RATING/SHARE ROUTINE                  
*                                                                               
DPJD30   OI    6(R5),X'80'         TRANSMIT BOTH LINES                          
         OI    6(R2),X'80'                                                      
         OI    4(R5),X'20'         MARK INPUT VALIDATED                         
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R3,L'SVPJPRGS(R3)                                                
         LA    R4,8(R4)                                                         
         LA    R5,COMPG2H-COMPG1H(R5)                                           
         LA    R2,COMRS2H-COMRS1H(R2)                                           
         BCT   R0,DPJD10           DO FOR ALL STATIONS                          
*                                                                               
         B     XIT                                                              
***********************************************************************         
* DISPLAY THE RATING SERVICE                                          *         
***********************************************************************         
DISSRC   NTR1                                                                   
         MVC   BYTE,SVRTGSVC                                                    
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   BYTE,CLTSRC                                                      
         OI    COMSRCH+6,X'80'                                                  
***  CABLE/FUSION                                                               
         MVC   COMSRC,=C'FUS'                                                   
         CLI   BYTE,C'F'                                                        
         BE    DISSRCX                                                          
***  CABLE/FUSION                                                               
         MVC   COMSRC,=C'NSI'                                                   
         CLI   BYTE,C'N'                                                        
         BE    *+10                                                             
         MVC   COMSRC,=C'ARB'                                                   
         CLI   AGENCY,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   COMSRC,=C'CSI'                                                   
         CLI   BYTE,C'N'                                                        
         BE    DISSRCX                                                          
         MVC   COMSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       GETSTAS                                       *         
***********************************************************************         
GETSTAS  NTR1                                                                   
*                                                                               
***      MVC   SVSTA(5),QSTA       FIND STATIONS IN THE MARKET                  
         MVC   SVSTA(8),QSTANEW    FIND STATIONS IN THE MARKET                  
***  CABLE/FUSION                                                               
         CLI   QSTANEW,C'0'        WE HAVE A CABLE NETWORK?                     
         BNL   GSTA70               - YUP, GET STATIONS FROM DEMAND             
***  CABLE/FUSION                                                               
         MVC   SVSTA+5(3),SPACES                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY              READ STATION LIST RECORD                     
         USING CLSRECD,R4                                                       
         MVI   CLSKTYP,CLSKTYPQ                                                 
         MVI   CLSKSUB,CLSKSUBQ                                                 
         MVC   CLSKAGMD,BAGYMD                                                  
         MVC   CLSKMKT,BMKT                                                     
         MVC   CLSKSCHM,CMPPSCHM                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BE    GSTA10                                                           
         MVC   KEY,KEYSAVE          TRY WITHOUT NSID SCHEME                     
         XC    CLSKSCHM,CLSKSCHM                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST STATION EXISTS                         
         BNE   GSTA70              NOT FOUND-GET STATIONS FROM DEMAND           
*                                                                               
GSTA10   GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    RE,L'CLSSTA         FOUND - EXTRACT STATIONS FROM                
         ZIC   RF,CLSELLN                  STATION LIST RECORD                  
         LA    RF,CLSEL(RF)                                                     
         BCTR  RF,0                                                             
         LA    R4,CLSSTA                                                        
         LA    R3,SVSTA+L'SVSTA                                                 
         LA    R6,1                R6=STATION COUNT                             
*                                                                               
GSTA20   CLC   QSTANEW(L'CLSSTA),0(R4)                                          
         BE    GSTA30                                                           
         MVC   0(5,R3),0(R4)                                                    
         MVC   5(3,R3),SPACES                                                   
         LA    R3,L'SVSTA(R3)                                                   
         LA    R6,1(R6)                                                         
*                                                                               
GSTA30   BXLE  R4,RE,GSTA20                                                     
*                                                                               
         L     R4,AIO              LOOK FOR SPILL STATIONS                      
         LA    R2,CLSEL                                                         
         SR    R0,R0                                                            
*                                                                               
GSTA40   CLI   0(R2),0                                                          
         BE    GSTA60                                                           
         CLI   0(R2),CSPELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GSTA40                                                           
         LA    R4,L'CSPSTA         FOUND-EXTRACT SPILL STATIONS                 
         ZIC   R5,1(R2)                                                         
         AR    R5,R2                                                            
         BCTR  R5,0                                                             
         LA    R2,CSPSTA-CSPEL(R2)                                              
*                                                                               
GSTA50   MVC   0(5,R3),0(R2)       SAVE SPILL STATION                           
         MVC   5(3,R3),SPACES                                                   
         MVC   8(2,R3),MKTRS       SAVE RATING SERVICE MARKET NUMBER            
         LA    R3,L'SVSTA(R3)                                                   
         LA    R6,1(R6)                                                         
         BXLE  R2,R4,GSTA50                                                     
*                                                                               
GSTA60   STC   R6,SVSTANO                                                       
         CLI   SVSTANO,NMAXSTA                                                  
         BNH   GSTAX                                                            
         DC    H'0'                BLOW IF TOO MANY STATIONS                    
         DROP  R4                                                               
*                                                                               
GSTA70   XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIO3                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,QMED                                                    
         MVC   DBSELSRC,CLTSRC                                                  
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   DBSELSRC,SVRTGSVC                                                
         MVC   DBSELSTA,QSTA                                                    
         LA    R1,SVBKS+6                                                       
         LA    R3,SVBKTPS+3                                                     
*                                                                               
GSTA80   OC    0(2,R1),0(R1)                                                    
         BNZ   GSTA90                                                           
         SHI   R1,2                                                             
         SHI   R3,1                                                             
         LA    R0,SVBKS                                                         
         CR    R1,R0                                                            
         BNL   GSTA80                                                           
         LA    R1,=X'570B'                                                      
*                                                                               
GSTA90   MVC   DBSELBK,0(R1)                                                    
*                                                                               
         TM    DBSELBK+1,BTYBITSQ   ANY SPECIAL BOOK TYPE?                      
         BZ    GSTA95                                                           
         TM    DBSELBK+1,BTY2CHAR   2 CHARACTER BOOKTYPE?                       
         BNO   GSTA92R                                                          
         CLI   0(R3),0             ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DBBTYPE,0(R3)                                                    
         B     GSTA92T                                                          
*                                                                               
GSTA92R  LA    RE,DBSELBK+1                                                     
         ST    RE,DMCB                                                          
         MVI   DMCB,C'B'                                                        
         LA    RE,DBBTYPE                                                       
         ST    RE,DMCB+4                                                        
         LR    R0,R1                                                            
         LA    R1,DMCB                                                          
         BRAS  RE,GTBKTYPE                                                      
         LR    R1,R0                                                            
GSTA92T  NI    DBSELBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
GSTA95   MVI   DBFUNCT,DBGETMK     GET THE RATING SERVICE MARKET                
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0,0                                             
*                                                                               
***  CABLE/FUSION                                                               
         CLI   QSTA,C'0'           WE DOING CABLE?                              
         BL    GSTA95C              - NOPE, NOT REALLY                          
         XC    DBBTYPE,DBBTYPE     WIPE OUT THE BOOKTYPE                        
         B     GSTA95E                                                          
***  CABLE/FUSION                                                               
GSTA95C  OC    DBACTRMK,DBACTRMK                                                
         BZ    ERRNODEM                                                         
GSTA95E  OC    MKTRS,MKTRS         ALREADY GOT RTG SVC MKT?                     
         BZ    GSTA98              IF NOT, THEN USE WHAT DEMAND HAS             
         CLC   MKTRS,DBACTRMK      SAME AS WHAT DEMAND HAS?                     
         BE    GSTA98              YES, NO PROBLEM                              
         MVC   DBACTRMK,MKTRS      USE WHAT WE GOT EARLIER                      
*                                                                               
GSTA98   MVI   DBFUNCT,DBGETMS     GET THE STATIONS                             
         MVC   DBSELRMK,DBACTRMK                                                
         MVI   SVSTANO,1                                                        
***  CABLE/FUSION                                                               
***      CLI   QSTA,C'0'                                                        
***      BL    GSTA98G                                                          
***      CLI   SVRTGSVC,C'N'                                                    
***      BNE   GSTA98G                                                          
***      MVI   DBBTYPE,C'W'        WE NEED NSI WIRED STATION LIST               
***  CABLE/FUSION                                                               
GSTA98G  L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,STAHOOK,0                                       
*                                                                               
GSTAX    B     XIT                                                              
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                          *         
***********************************************************************         
         SPACE 1                                                                
STAHOOK  LR    R0,RE               SAVE OFF CURRENT RE FOR EXITING              
         L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
***  CABLE/FUSION                                                               
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    STAH05               - NOPE, REGULAR STATION                     
**                                                                              
         XC    WORK,WORK           CLEAR OUT APWORK + EXTENSION                 
         CLI   SVRTGSVC,C'F'       WE DOING FUSION?                             
         BNE   STAH02               - NOPE, MUST BE NSI WIRED                   
         CLI   DBACTSTA,C'0'       IS IT A NUMBER                               
         BNL   STAHX                - YUP, IGNORE                               
         MVC   WORK+60(4),DBACTSTA   SAVE OFF THE NETWORK                       
         B     STAH03                                                           
**                                                                              
STAH02   TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    STAHX                - YUP, IGNORE                               
         MVC   WORK+60(4),MLSTAT   SAVE OFF THE NETWORK                         
         MVI   DBBTYPE,C'W'        WE NEED NSI WIRED STATION LIST               
***  CALL STAPACK TO CONVERT TO 3 BYTE NETWORK AND CHECK IF IT BELONGS          
STAH03   LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'Y'        TRANSLATE 4CHAR NET TO 3CHAR                 
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQSTA,WORK+60                                                 
         GOTO1 VSTAPACK,(R1)                                                    
*                                                                               
         CLC   STAPQNET,=C'   '    WE GOT ANY NETWORK CODE?                     
         BE    STAHX                - NOPE, GET OUTTA HERE                      
         CLC   QSTANEW+5(3),STAPQNET   COMPARE 3 BYTES                          
         BE    STAHX                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(3),STAPQNET     NEED THIS STORED TEMPORARILY HERE           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'        SEE IF IT BELONGS TO SYSCODE                 
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET,FULL                                                    
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BNE   STAHX                                                            
         DROP  R1                                                               
***  CALL STAPACK TO CONVERT TO 3 BYTE NETWORK AND CHECK IF IT BELONGS          
*                                                                               
         B     STAH08                                                           
***  CABLE/FUSION                                                               
STAH05   OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   STAHX               YES - IGNORE                                 
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    STAHX               YES - IGNORE                                 
         CLC   QSTA(5),MLSTAT      TEST STATION = REQUEST STATION               
         BE    STAHX               YES - IGNORE (ALREADY FIRST IN LIST)         
STAH08   CLI   SVSTANO,NMAXSTA                                                  
         BL    STAH10                                                           
*****    DC    H'0'                BLOW IF TOO MANY STATIONS                    
         OI    SVFLAG,SVF2STA      TOO MANY STATIONS TO LIST (>20)              
         B     STAHX                                                            
*                                                                               
STAH10   ZIC   R1,SVSTANO                                                       
         LR    RF,R1                                                            
         MHI   RF,L'SVSTA                                                       
         LA    RF,SVSTA(RF)                                                     
***  CABLE/FUSION                                                               
         CLI   QSTA,C'0'                                                        
         BL    STAH10G                                                          
         XC    0(L'SVSTA,RF),0(RF)                                              
         MVC   0(5,RF),QSTA                                                     
         MVC   5(3,RF),FULL                                                     
         B     STAH10K                                                          
***  CABLE/FUSION                                                               
STAH10G  MVC   0(5,RF),MLSTAT                                                   
         MVC   5(3,RF),SPACES                                                   
STAH10K  LA    R1,1(R1)                                                         
         STC   R1,SVSTANO                                                       
STAHX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                       ERROR MESSAGES                                *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBYR   MVC   ERRNUM,=AL2(INVBYRN)                                             
         B     SPERREX                                                          
ERRCAM   MVC   ERRNUM,=AL2(INVCAM)                                              
         B     SPERREX                                                          
ERREST   MVC   ERRNUM,=AL2(INVESTE)                                             
         B     SPERREX                                                          
ERRDAY   MVC   ERRNUM,=AL2(INVDAYS)                                             
         B     SPERREX                                                          
ERRTIM   MVC   ERRNUM,=AL2(INVTIM)                                              
         B     SPERREX                                                          
ERRDPT   MVC   ERRNUM,=AL2(INVDPTC)                                             
         B     SPERREX                                                          
ERRMSLN  MVC   ERRNUM,=AL2(MISSLN)                                              
         B     SPERREX                                                          
ERRSLN   MVC   ERRNUM,=AL2(INVSLN)                                              
         B     SPERREX                                                          
ERRDEM1  MVC   ERRNUM,=AL2(INVDEM1)                                             
         B     SPERREX                                                          
ERRDEM2  MVC   ERRNUM,=AL2(INVDEM2)                                             
         B     SPERREX                                                          
ERRBOOK  MVC   ERRNUM,=AL2(INVBOOK)                                             
         B     SPERREX                                                          
ERRNODEM MVC   ERRNUM,=AL2(NODEMO)                                              
         B     SPERREX                                                          
ERR21P   MVC   ERRNUM,=AL2(TWO1PLUS)                                            
         B     SPERREX                                                          
                                                                                
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
INVBYRN  EQU   1021                INVALID BUYER NAME                           
INVCAM   EQU   1001                INVALID CAMPAIGN NUMBER                      
INVESTE  EQU   42                  ESTIMATE NOT IN FILE                         
INVDAYS  EQU   47                  INVALID DAY EXPRESSION                       
INVTIM   EQU   48                  INVALID DAY EXPRESSION                       
INVDPTC  EQU   21                  INVALID DAYPART EXPRESSION                   
MISSLN   EQU   243                 MISSING SPOT LENGTH                          
INVSLN   EQU   1007                INVALID SPOT LENGTH                          
INVDEM1  EQU   388                 INVALID DEMO                                 
INVDEM2  EQU   286                 DEMO NOT FOUND IN ESTIMATE RECORD            
INVBOOK  EQU   60                  INVALID BOOK                                 
NODEMO   EQU   327                 NO DEMO INFORMATION IS AVAILABLE             
TWO1PLUS EQU   1322                                                             
         EJECT                                                                  
***********************************************************************         
*                       GETEL                                         *         
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
*                       LTORG                                         *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       DISDEMS                                       *         
***********************************************************************         
DISDEMS  NTR1  BASE=*,LABEL=*                                                   
*                                  FORMAT STATIONS                              
         CLC   STANDIS,SVSTANO                                                  
         BL    *+8                                                              
         MVI   STANDIS,0                                                        
*                                                                               
         LA    R0,7                # OF STATION CAN BE DISPLAYED                
         ZIC   RE,STANDIS                                                       
         ZIC   RF,SVSTANO                                                       
         SR    RF,RE                                                            
         CR    RF,R0               FULL SCREEN?                                 
         BNL   *+6                 YES                                          
         LR    R0,RF               ACTUAL # STATION ON DISPLAY                  
         STC   R0,NOSTA                                                         
*                                                                               
         LA    R1,7                                                             
         SR    R1,R0               # OF NULL STATION LINES ON DISPLAY           
*                                                                               
         LA    R0,7                # OF STATION CAN BE DISPLAYED                
         MH    RE,=Y(L'SVSTA)                                                   
         LA    RE,SVSTA(RE)        TARGET STATION                               
         LA    R6,COMLN2H                                                       
         LA    R4,COMLN1H                                                       
         LA    R5,COMLN1                                                        
         USING LINE1D,R5                                                        
*                                                                               
DDEM10   CR    R0,R1               NULL LINE?                                   
         BH    DDEM15              NO                                           
*                                                                               
         XC    8(L1LENQ,R4),8(R4)  CLEAR THE ENTIRE LINE                        
         XC    8(L2LENQ,R6),8(R6)  CLEAR THE ENTIRE LINE                        
         B     DDEM20                                                           
*                                                                               
DDEM15   OC    0(8,RE),0(RE)                                                    
         BZ    DDEM30                                                           
         MVC   L1STA(8),0(RE)                                                   
         CLI   0(RE),C'0'          TEST CABLE                                   
         BL    *+12                                                             
         MVI   L1STA+4,C'/'        YES                                          
         B     DDEM20                                                           
***  CABLE/FUSION                                                               
         OC    0(5,RE),0(RE)       WE DOING CABLE?                              
         BNZ   DDEM15G                                                          
         MVI   L1STA+4,C'/'                                                     
         MVC   L1STA+5(4),5(RE)                                                 
         B     DDEM20                                                           
***  CABLE/FUSION                                                               
DDEM15G  MVC   L1STA+4(4),=C'-TV '                                              
         CLI   4(RE),C'T'                                                       
         BE    *+14                                                             
         MVC   L1STA+5(1),4(RE)                                                 
         MVI   L1STA+6,C'M'                                                     
         CLI   3(RE),C' '                                                       
         BNE   *+14                                                             
         MVC   L1STA+3(3),L1STA+4                                               
         MVI   L1STA+6,C' '                                                     
*                                                                               
DDEM20   OI    6(R4),X'80'         TRANSMIT BOTH STATION LINES                  
         OI    6(R6),X'80'                                                      
*                                                                               
         LA    R4,COMLN3H-COMLN1H(R4)                                           
         LA    R5,COMLN3H-COMLN1H(R5)                                           
         LA    R6,COMLN3H-COMLN1H(R6)                                           
*                                                                               
         LA    RE,L'SVSTA(RE)                                                   
         BCT   R0,DDEM10           DO FOR ALL STATIONS                          
         DROP  R5                                                               
*                                  FORMAT PROGRAM NAMES                         
DDEM30   ZIC   R4,STANDIS                                                       
         MH    R4,=Y(4*L'L1PROG1)                                               
         LA    R4,SVPROGS(R4)      TARGET PROGRAM NAME                          
         LA    R5,COMLN1                                                        
         USING LINE1D,R5                                                        
         ZIC   R0,NOSTA            # STATIONS ON DISPLAY                        
*                                                                               
DDEM40   LA    RE,L1PROG1                                                       
         LA    RF,4                                                             
*                                                                               
DDEM50   MVC   0(L'L1PROG1,RE),0(R4)                                            
         LA    R4,L'L1PROG1(R4)                                                 
         LA    RE,L1PROG2-L1PROG1(RE)                                           
         BCT   RF,DDEM50           DO FOR ALL BOOKS                             
*                                                                               
         LA    R5,COMLN3H-COMLN1H(R5)                                           
         BCT   R0,DDEM40           DO FOR ALL STATIONS                          
*                                                                               
*                                  FORMAT RATINGS AND SHARES                    
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         ZIC   R4,STANDIS                                                       
         SLL   R4,5                X32                                          
         LA    R4,SVDEMVAL(R4)     R4 = A(RTG/SHR)                              
*                                                                               
         ZIC   R0,NOSTA                                                         
         LA    R2,COMLN2                                                        
         USING LINE2D,R2                                                        
*                                                                               
DDEM60   LA    R6,L2RS1                                                         
         LA    RF,4                                                             
*                                                                               
DDEM70   XC    0(L'L2RS1,R6),0(R6)                                              
         BRAS  RE,FMTRS            FORMAT RATING/SHARE ROUTINE                  
         LA    R6,L2RS2-L2RS1(R6)                                               
         LA    R4,8(R4)                                                         
         BCT   RF,DDEM70           DO FOR ALL BOOKS                             
*                                                                               
         LA    R2,COMLN3-COMLN1(R2)                                             
         BCT   R0,DDEM60           DO FOR ALL STATIONS                          
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       SDEMUP                                        *         
***********************************************************************         
***********************************************************************         
* SINGLE DEMO UPGRADE(LOOK UP) ROUTINE                                *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
***********************************************************************         
SDEMUP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LDEMVAL,LDEMVAL                                                  
         OC    SVUGRD,SVUGRD                                                    
         BZ    SDEMX                                                            
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,AIO3                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPLPM,SVMLPMSD                                                 
         MVC   SPUPMALF,SVMALPHA                                                
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
         MVC   SPUPMED,QMED                                                     
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,BMKT                                                     
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTANEW,C'0'        IS IT A NUMBER?                              
         BL    SDEM5E               - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTANEW+5   MOVE THE NETWORK IN                       
         MVC   SPUPSYSE,QSTANEW                                                 
         B     SDEM5G                                                           
*****  CABLE/FUSION DATE LOOKUP                                                 
SDEM5E   MVC   SPUPSTA,QSTA                                                     
SDEM5G   MVC   SPUPDAY,DAY                                                      
         MVC   SPUPTIM,TIME                                                     
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUFRBK                                                  
         MVC   SPUPFBKL,SVUFBL                                                  
         MVC   SPUPUDAY,SVUDAY                                                  
         MVC   SPUPUTIM,SVUTIM                                                  
*                                                                               
         CLI   QMED,C'C'          TEST CANADA                                   
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         MVC   SPUPTYPE(L'SVUGRD),SVUGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         TM    SPUPFBK+1,BTYBITSQ  ANY SPECIAL BOOK?                            
         BZ    SDEM10              NO                                           
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPES?                       
         BNO   SDEM08R                                                          
         CLI   SVUFRBT,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,SVUFRBT                                                 
         B     SDEM08T              - YUP, ALREADY HAVE QBOOKTYP                
*                                                                               
SDEM08R  GOTOR =A(GTBKTYPE),DMCB,(C'B',SPUPFBK+1),SPUPBTYP,RR=RELO              
SDEM08T  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
SDEM10   CLI   SVUPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    SDEM15               - NOPE                                      
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
SDEM15   GOTO1 VSPDEMUP,DMCB,LDMUPBLK,LDEMLST,LDEMVAL                           
***  WHY WAS THE FOLLOWING LINE COMMENTED OUT??                                 
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
***                                                                             
*                                                                               
SDEMX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       INSPGS                                        *         
***********************************************************************         
INSPGS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   R0,NOSTA            # STATIONS ON THIS SCREEN                    
         LA    R2,COMPG1H                                                       
         LA    R3,SVPJPRGS         SAVED PJ PROGRAM NAME LIST                   
         ZIC   RE,STANDIS                                                       
         MHI   RE,L'SVPJPRGS                                                    
         AR    R3,RE               TARGET PROGRAM NAME                          
*                                                                               
IPGS10   TM    4(R2),X'20'         ANY CHANGE?                                  
         BO    IPGS20              NO - SKIP                                    
*                                                                               
         OI    LCHG,LCPGS                                                       
         MVC   0(L'L1PJPROG,R3),8(R2)                                           
         LR    R6,R0                                                            
         GOTOR TONWSTBL,DMCB,(X'80',(R2))                                       
*                                                                               
IPGS20   LA    R2,COMPG2H-COMPG1H(R2)                                           
         LA    R3,L'SVPJPRGS(R3)                                                
         BCT   R0,IPGS10           NEXT PROGRAM NAME                            
*                                                                               
IPGSX    J     XIT                                                              
***********************************************************************         
* THIS ROUTINE PUTS THE STATION INTO  NWSTBL BECAUSE THE RATING/PROGRAM         
*     WAS CHANGED FOR THAT STATION                                              
*                                                                               
* ON ENTRY:    PARAM 1  BYTE 0     X'80' - PROGRAM NAME CHANGE                  
*                       BYTES 1-3  A(PROGRAM NAME)                              
*                    - OR -                                                     
*                       BYTES 0-3  ALL ZEROS - RATING CHANGE                    
*                                                                               
*              (R6)                # OF STA LEFT TO VALIDATE ON SCREEN          
*              NOSTA               # OF STATIONS ON THIS SCREEN                 
*              STANDIS             # OF STA IN SVSTA FOR STA ON 1ST LN          
***********************************************************************         
TONWSTBL NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2 = A(PROGRAM NAME)                         
         ZIC   RE,NOSTA            FIGURE OUT WHICH STATION HAS RATING          
         SR    RE,R6                  CHANGED                                   
         ZIC   R6,STANDIS                                                       
         AR    RE,R6                                                            
         MHI   RE,L'SVSTA                                                       
         LA    RF,SVSTA(RE)                                                     
*                                                                               
         L     R1,ANWSTBL                                                       
         USING NWSTBLD,R1                                                       
         LA    RE,NMAXSTA*NTBLLENQ(R1)                                          
TNWST10  OC    0(NTBLLENQ,R1),0(R1)   EMPTY SLOT?                               
         BZ    TNWST20                YES, STICK STATION HERE                   
*                                                                               
         CLC   NTBLQSTA,0(RF)      STATION ALREADY IN TABLE?                    
         BE    TNWST20             YES, COULD BE A DIFFERENT CHANGE             
*                                                                               
         LA    R1,NTBLLENQ(R1)                                                  
         CR    R1,RE                                                            
         BL    TNWST10                                                          
         DC    H'0'                DIE, DON'T MESS UP THE D-CHAIN               
*                                                                               
TNWST20  MVC   NTBLQSTA,0(RF)                                                   
*                                                                               
         LTR   R2,R2               IS THIS A PROGRAM NAME CHANGE?               
         BNZ   *+12                                                             
         OI    NTBLFLAG,NTBLFRTG   NO, RATING CHANGE                            
         B     TNWSTX                                                           
*                                                                               
         OI    NTBLFLAG,NTBLFPGM   YES                                          
         DROP  R1                                                               
*                                                                               
TNWSTX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FORMAT PROJECTED CUM SHARE                                          *         
***********************************************************************         
FMTPJCUM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUMPROJ                                                 
         MVI   EBDECS,1                                                         
         LA    R1,SVPJCUM                                                       
         ST    R1,EBAIN                                                         
         LA    R4,COMCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R1,CUMPROJ                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 EDITOR,DMCB,EBLOCK                                               
         OI    COMCUMH+6,X'80'                                                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT RATING/SHARE                                      *         
* INPUT  : R4 = A(RTG/SHR)                                            *         
*          R6 = A(FORMAT AREA)                                        *         
***********************************************************************         
FMTRS    NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB,0(R4)           DUB=RATING/SHARE                             
         LA    R3,DUB                                                           
         NI    0(R3),255-X'80'                                                  
         NI    4(R3),255-X'80'                                                  
         LA    RE,L'COMRS1                                                      
         CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARE                          
         BE    FMTR5               YES-RATING TAKES UP WHOLE FIELD              
         LA    RE,4                RE=L'SHARE FIELD                             
         TM    4(R4),X'80'                                                      
         BZ    FMTR1                                                            
         TM    SVPJPUT,X'80'       IF PUT & RATING ARE NOT OVERRIDE             
         BNZ   FMTR2                                                            
         TM    0(R4),X'80'                                                      
         BNZ   FMTR2                                                            
FMTR1    CLC   4(4,R3),=F'100'     THEN SHARE SHOULDN'T BE  (MAYA)              
         BNL   FMTR4                                                            
         BCTR  RE,0                                                             
         B     FMTR4                                                            
*                                                                               
FMTR2    CLC   4(4,R3),=F'100'                                                  
         BL    FMTR4                                                            
         LA    RE,1(RE)                                                         
*                                                                               
FMTR4    STC   RE,BYTE             BYTE=L'SHARE FIELD                           
         LNR   RE,RE                                                            
         LA    RF,L'COMRS1-1                                                    
         AR    RE,RF                                                            
*                                                                               
FMTR5    STC   RE,EBLOUT           FORMAT THE RATING                            
         OC    0(4,R3),0(R3)                                                    
         BNZ   FMTR6                                                            
         SHI   RE,3                                                             
         AR    RE,R6                                                            
         MVC   0(3,RE),=C'0.0'                                                  
         B     FMTR8                                                            
*                                                                               
FMTR6    MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'         TEST OVERRIDE                                
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         ST    R6,EBAOUT                                                        
         LA    R1,DUB                                                           
         ST    R1,EBAIN                                                         
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    0(R4),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         MVI   EBDECS,2                                                         
         NI    DUB,X'FF'-DMODEM2D   TAKE OFF THE BIT FOR CORRECT VALUE          
***  2 DECIMAL  ***                                                             
         MVI   EBSCIN,0                                                         
         GOTO1 EDITOR,DMCB,EBLOCK                                               
         MVI   EBDECS,1            GOTTA PUT IT BACK TO 1 DECIMAL               
*                                                                               
FMTR8    CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARES                         
         BE    FMTRX                                                            
         ZIC   RE,EBLOUT           NO-FORMAT THE SHARE                          
         AR    R6,RE                                                            
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         ST    R6,EBAOUT                                                        
         LA    R1,4(R3)                                                         
         ST    R1,EBAIN                                                         
         OC    4(4,R3),4(R3)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R6),=C'0.0'                                                  
         B     FMTRX                                                            
         MVC   EBLOUT,BYTE                                                      
         MVI   EBFLOAT,0                                                        
         TM    4(R4),X'80'                                                      
         BZ    FMTR9                                                            
         TM    SVPJPUT,X'80'       IF PUT & RATING ARE NOT OVERRIDDEN           
         BNZ   *+12                                                             
         TM    0(R4),X'80'                                                      
         BZ    FMTR9               THEN SHARE SHOULDN'T BE   (MAYA)             
         MVI   EBFLOAT,C'*'                                                     
FMTR9    GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
FMTRX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CALCULATES THE FIELD LENGTH                                                   
***********************************************************************         
CALCFLEN NTR1  BASE=*,LABEL=*                                                   
         L     R2,DMCB             R2 = A(FIELD HEADER)                         
         LA    R2,0(R2)                                                         
         XR    RF,RF               RF = L(FIELD)                                
         IC    RF,DMCB                                                          
         LA    RE,7(RF,R2)         RE = A(LAST BYTE IN THE FIELD)               
CFLEN10  CLI   0(RE),C' '                                                       
         BH    CFLEN20                                                          
         BCTR  RE,0                                                             
         BCT   RF,CFLEN10                                                       
CFLEN20  STC   RF,5(R2)                                                         
         OI    6(R2),X'80'                                                      
CFLENX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COPIED FROM SPNWS00                                                           
* VALIDATE/GET SPECIAL BOOK TYPE CODE                                           
*                                                                               
* ON ENTRY:    1ST PARAM BYTE  0   FORMAT, C'B' BINARY, C'C' CHARACTER          
*                        BYTES 1-3 A(BOOK TYPE CODE)                            
*              2ND PARAM           A(RETURN BOOK TYPE CODE)                     
* RETURN CODE IS OPPOSITE FORMAT TO INPUT - IF BINARY CODE INPUT THEN           
* THE CORRESPONDING CHAR CODE RETURNED. IF P2=A(0) NO CODE IS RETURNED          
***********************************                                             
* ON EXIT :    1ST APPARM BYTE 0   SET TO X'FF' IF INVALID CODE                 
***********************************************************************         
GTBKTYPE NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)            A(INPUT CODE)                                
         MVC   BYTE,0(R5)          COPY BOOK TYPE (BIN OR EBCDIC)               
         LR    R2,R5               FOR CHECKS IN GTBK20                         
         LR    R5,R1               USE IT FOR 2 CHARACTER STUFF                 
         L     RF,4(R1)            A(OUTPUT CODE)                               
*                                                                               
         CLI   BYTE,C'+'           IGNORE LPM?                                  
         BE    GTBKXIT              - YUP                                       
*                                                                               
         LA    RE,BKTYPTAB                                                      
         CLI   0(R1),C'C'          FORMAT                                       
         BE    GTBK20                                                           
         CLI   0(R1),C'B'                                                       
         BNE   GTBKERR             INVALID FORMAT - RETURN ERROR                
* BINARY -> EBCDIC CODE                                                         
         NI    BYTE,X'FF'-BMNBITSQ   REMOVE ANY MONTH BITS                      
         CLI   BYTE,BTY2CHAR       X'E0' - 2 CHARACTER BOOKTYPES?               
         BE    GTBK40               - YES, GO THERE                             
*                                                                               
GTBK10   CLI   0(RE),0                                                          
***      BE    GTBKERR             NOT IN TABLE!                                
         BE    GTBK40              NOT IN TABLE!  TRY DEMTABS                   
         CLC   BYTE,0(RE)                                                       
         BE    *+12                                                             
         LA    RE,L'BKTYPTAB(RE)                                                
         B     GTBK10                                                           
         LTR   RF,RF               RETURN CHARACTER CODE IF REQUIRED            
         BZ    *+10                                                             
         MVC   0(1,RF),1(RE)                                                    
         B     GTBKXIT                                                          
* EBCIDIC CODE                                                                  
GTBK20   DS    0H                                                               
         CLI   1(R2),0             SOMETHING IN THE 2ND BYTE?                   
         BNE   GTBK40               - YUP                                       
*                                                                               
GTBK20E  CLI   0(RE),0                                                          
***      BE    GTBKERR             NOT IN TABLE!                                
         BE    GTBK40              NOT IN TABLE!  TRY DEMTABS                   
         CLC   BYTE,1(RE)                                                       
         BE    *+12                                                             
         LA    RE,L'BKTYPTAB(RE)                                                
         B     GTBK20                                                           
         LTR   RF,RF               RETURN BINARY CODE IF REQUIRED               
         BZ    *+10                                                             
         OC    0(1,RF),0(RE)       NOTE: 'OR' TO PRESERVE MONTH BITS!           
         B     GTBKXIT                                                          
*                                                                               
***************************     2 CHARACTER BOOKTYPES     MHC 11/28/06          
GTBK40   DS    0H                                                               
         L     R2,0(R5)            A(INPUT CODE)                                
         L     R3,4(R5)            A(OUTPUT CODE)                               
         L     R4,8(R5)            A(OUTPUT 2 CHAR)                             
         MVC   BYTE,0(R5)          SAVE OFF THE LETTER                          
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,SPBOOKTB   GET A(BOOK TABLE)                           
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
         CLI   BYTE,C'C'           FORMAT                                       
         BE    GTBK60                                                           
         CLI   BYTE,C'B'                                                        
         BNE   GTBKERR             INVALID FORMAT - RETURN ERROR                
*                                                                               
GTBK50   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),SPBKTYPN    R4 HAS THE ALPHANUMERIC                      
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK50                                                           
*                                                                               
         MVC   0(2,R3),SPBKTYPA    BOOK TYPE                                    
         B     GTBKXIT                                                          
*                                                                               
GTBK60   DS    0H                                                               
         CLI   1(R2),0             IS IT NULL?                                  
         BNE   *+8                                                              
         MVI   1(R2),C' '          MAKE IT A SPACE INSTEAD FOR TABLE            
GTBK62   CLI   0(RF),X'FF'                                                      
         BE    GTBKERR                                                          
         CLC   0(2,R2),SPBKTYPA                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK62                                                           
*                                                                               
         CLI   SPBKTYPN,0          STANDARD (NO BOOK TYPE)?                     
         BE    GTBK65              NO BOOK TYPE                                 
*                                                                               
         LTR   R3,R3               ANYTHING HERE?                               
         BZ    GTBK65               - NOPE                                      
         OI    0(R3),BTY2CHAR      WE ARE DOING 2 CHARACTERS                    
GTBK65   MVC   0(1,R4),SPBKTYPN                                                 
         DROP  RF                                                               
         B     GTBKXIT                                                          
***************************     2 CHARACTER BOOKTYPES     MHC 11/28/06          
*                                                                               
GTBKERR  MVI   0(R1),X'FF'                                                      
*                                                                               
GTBKXIT  XIT1  ,                                                                
         LTORG                                                                  
*                                                                               
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: SOME ROOT ROUTINES DO NOT ACCESS THIS TABLE AND HAVE OWN CODE           
BKTYPTAB DS    0XL2                 SEE BOOKTYPD FOR SETTINGS                   
         DC    AL1(BTYZR4Q),C'4'    - ZERO 4                                    
         DC    AL1(BTYZR1Q),C'1'    - ZERO 1                                    
         DC    AL1(BTYHPEOQ),C'I'   - HISPANIC PEOPLE METER                     
         DC    AL1(BTYOLYMQ),C'O'   - OLYMPICS                                  
         DC    AL1(BTYPRNTQ),C'A'   - PARENT ONLY DATA                          
         DC    AL1(BTYHISPQ),C'H'   - HISPANIC                                  
         DC    AL1(BTYBLAKQ),C'B'   - BLACK                                     
         DC    AL1(BTYPEOPQ),C'P'   - PEOPLE METER                              
         DC    AL1(BTYTRADQ),C'T'   - TRADE                                     
         DC    AL1(BTYMTROQ),C'M'   - METRO                                     
         DC    AL1(BTYDMAQ),C'D'    - DMA                                       
         DC    AL1(BTYOTHRQ),C'E'   - OTHER                                     
         DC    AL1(BTY2CHAR),X'FE'  - 2 CHARACTER BOOKTYPES                     
         DC    AL1(0)                                                           
***************************************                                         
*  COPIED FROM SPNWSWRK                                                         
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: SOME ROOT ROUTINES DO NOT ACCESS THIS TABLE AND HAVE OWN CODE           
***************************************                                         
BMNBITSQ EQU   X'0F'               MONTH BITS                                   
BTYBITSQ EQU   X'F0'               SPECIAL TYPE BITS                            
BTY2CHAR EQU   X'80'+X'40'+X'20'   - 2 CHARACTER BOOKTYPES                      
BTYZR4Q  EQU   X'80'+X'40'         - ZERO 4                                     
BTYZR1Q  EQU   X'80'+X'20'+X'10'   - ZERO 1                                     
BTYHPEOQ EQU   X'80'+X'20'         - HISPANIC PEOPLE METER                      
BTYPRNTQ EQU   X'80'+X'10'         - PARENT ONLY DATA                           
BTYOLYMQ EQU   X'80'               - OLYMPICS                                   
BTYHISPQ EQU   X'40'+X'20'+X'10'   - HISPANIC                                   
BTYBLAKQ EQU   X'40'+X'20'         - BLACK                                      
BTYPEOPQ EQU   X'40'+X'10'         - PEOPLE METER                               
BTYTRADQ EQU   X'40'               - TRADE                                      
BTYMTROQ EQU   X'20'+X'10'         - METRO                                      
BTYDMAQ  EQU   X'20'               - DMA                                        
BTYOTHRQ EQU   X'10'               - OTHER                                      
         EJECT                                                                  
***********************************************************************         
*                       VALICAM                                                 
***********************************************************************         
*                              INPUT: R2=A(FIELD HDR OF CAMPAIGN FIELD)         
*                             OUTPUT: CLIENT   IN "BCLT"                        
*                                     PRODUCT  IN "BPRD,QPRD"                   
*                                     ESTIMATE IN "BEST"                        
*                                     SPOT LEN IN "CMPSLN"                      
*                                     RATING SRC OVERRIDE IN "CMPRSRC"          
*                                     DAYPART OPTION IN "CMPDPOPT"              
VALICAM  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         ZIC   RE,COMCAMH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,CAMINP1                                                       
         BE    *+12                                                             
         EX    RE,CAMINP2                                                       
         BNE   *+14                                                             
         XC    BCAM,BCAM                                                        
         B     VCAM40                                                           
*                                                                               
         ZIC   R3,COMCAMH+5        INPUT LENGTH                                 
         LA    RE,COMCAM-1(R3)     LAST DIGIT IN CAMPAIGN FIELD                 
VCAM10   CLI   0(RE),X'40'         IS THIS DIGIT SPACE/X'00'?                   
         BH    VCAM20              NO - NEXT                                    
         BCTR  RE,0                                                             
         BCT   R3,VCAM10                                                        
         B     ERRCAM                                                           
*                                  CHECK IF NUMERIC                             
VCAM20   LR    R4,R3                                                            
VCAM30   CLI   0(RE),C'0'                                                       
         BL    ERRCAM                                                           
         CLI   0(RE),C'9'                                                       
         BH    ERRCAM                                                           
         BCTR  RE,0                                                             
         BCT   R3,VCAM30                                                        
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,COMCAM(0)                                                    
         CVB   RE,DUB                                                           
         UNPK  COMCAM,DUB          REDISPLAY CAMPAIGN # W/ LEADING 0S           
         MVI   COMCAMH+5,5                                                      
*                                                                               
         STCM  RE,3,BCAM                                                        
         XC    BCAM,=X'FFFF'       ONES COMPLEMENT                              
*                                                                               
VCAM40   LA    R4,KEY              BUILD KEY OF CANPAIGN RECORD                 
         USING CAMRECD,R4                                                       
         XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK                                                
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   CAMKEY(CAMKCAM-CAMKEY),KEYSAVE                                   
         BNE   ERRCAM                                                           
         OC    BCAM,BCAM           TEST LATEST CAMPAIGN REQUESTED               
         BNZ   VCAM50                                                           
*                                                                               
         MVC   BCAM,CAMKCAM        SAVE THE CAMPAIGN #                          
*                                                                               
         MVC   HALF,BCAM           ONES COMPLEMENT                              
         XC    HALF,=X'FFFF'                                                    
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  COMCAM,DUB          REDISPLAY CAMPAIGN # W/ LEADING 0S           
         MVI   COMCAMH+5,5                                                      
         B     VCAM60                                                           
*                                                                               
VCAM50   CLC   CAMKEY(CAMKREST-CAMKEY),KEYSAVE                                  
         BNE   ERRCAM                                                           
         DROP  R4                                                               
*                                                                               
VCAM60   GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CAMRECD,R6                                                       
*                                                                               
         MVI   ELCODE,CAMELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ELEMENT                       
         USING CAMEL,R6                                                         
         MVC   BCLT,CAMCLT         CLIENT                                       
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         MVC   BPRD,CAMPRD         PRODUCT                                      
         MVC   QPRD,CAMPRDC        PRODUCT CODE                                 
         MVC   BEST,CAMEST         ESTIMATE                                     
         MVC   CMPSLN,CAMSLN       SPOT LEN                                     
         MVC   CMPOPTS,CAMOPT      OPTIONS                                      
         MVC   CMPUP,CAMUPGRD      UPGRADE                                      
         MVC   CMPRSVC,CAMRSVC     DEMO SOURCE OVERRIDE                         
         MVC   CMPDPOPT,CAMDPOPT   DAYPART OPTION                               
         MVC   CMPSCHEM,CAMSCHEM                                                
         OC    CMPSCHEM,CMPSCHEM                                                
         BZ    VCAM70                                                           
         GOTO1 CLPACK,DMCB,CMPSCHEM,CMPPSCHM                                    
*                                                                               
         MVC   CMPUP,CAMUPGRD      UPGRADE                                      
         MVC   CMPUF,CAMUPFIL                                                   
         MVC   CMPFB,CAMFRBK                                                    
         TM    CAMFRBK+1,BTY2CHAR                                               
         BNO   VCAM68                                                           
         CLI   CAMFRBKT,0                                                       
         BNE   *+10                                                             
         DC    H'0'                                                             
         CLI   CAMELLN,CAMELLNQ    EXPANDING LENGTH?                            
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   CMPFBTP,CAMFRBKT    CAMFRBK BOOKTYPE                             
VCAM68   MVC   CMPFBLST,CAMFRBKL                                                
         MVC   CMPUPIN,CAMINPUT                                                 
         MVC   CMPUPUT,CAMUPUT                                                  
         MVC   CMPUSHR,CAMUSHR                                                  
*                                                                               
VCAM70   CLI   CMPDPOPT,0          TEST CAMPAIGN SUBDPT OPTION NOT SET          
         BE    *+12                                                             
         TM    CMPOPTS,CAMOAALL+CAMOAIMP+CAMOATGT+CAMOANO  OR AUTOADJ           
         BNZ   VCAMX                                       NOT SET              
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),=C'BW'      CLIENT BW PROFILE                            
         GOTO1 PROFILE,DMCB,FULL,CLTBWPRO                                       
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),=C'1W'      1W PROFILE                                   
         GOTO1 PROFILE,DMCB,FULL,G1WPROF                                        
         CLI   G1WPROF+3,C'Y'     TEST CANADIAN ANGLO/FRANCO OPTION ON          
         BNE   *+8                                                              
         OI    CLTIND2,CLTIANFR    YES                                          
*                                                                               
         CLI   CMPDPOPT,0                                                       
         BNE   *+10                                                             
         MVC   CMPDPOPT,CLTBWPRO+4                                              
*                                                                               
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT+CAMOANO                       
         BNZ   VCAMX                                                            
         CLI   CLTBWPRO+3,C'I'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAIMP                                                 
         CLI   CLTBWPRO+3,C'A'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAALL                                                 
         CLI   CLTBWPRO+3,C'T'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOATGT                                                 
         DROP  R6                                                               
*                                                                               
VCAM80   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CKEY,R6                                                          
         CLI   CPROF+6,C'Y'                                                     
         BNE   VCAMX                                                            
         GOTO1 CLUNPK,DMCB,(C'Y',BCLT),QCLT                                     
         DROP  R6                                                               
*                                                                               
******  THIS CODE MUST BE DELETED WHEN IT GOES LIVE!!!  MHC 2D                  
***MX    MVI   SVAPROF+9,C'Y'                                                   
******  THIS CODE MUST BE DELETED WHEN IT GOES LIVE!!!                          
VCAMX    B     XIT                                                              
CAMINP1  CLC   COMCAM(0),=C'LAST  '     ** EXECUTED **                          
CAMINP2  CLC   COMCAM(0),=C'LATEST'                                             
***********************************************************************         
*                       PROFILE                                       *         
***********************************************************************         
*                              INPUT: P1=A(2 OR 3-BYTE PROFILE ID)              
*                                     P2=A(16-BYTE PROFILE AREA)                
*                             OUTPUT: RETURN PROFILE IN P2                      
PROFILE NTR1                                                                    
         LM    R2,R3,0(R1)                                                      
         BAS   RE,GETCLT           GET CLT OFFICE NUMBER                        
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'S0'                                                   
         MVC   WORK+2(2),0(R2)                                                  
         CLI   2(R2),C' '          TEST PROFILE ID 3 CHAR LONG                  
         BNH   *+14                                                             
         NI    WORK,255-X'40'      YES-MAKE LOWER CASE                          
         MVC   WORK+1(3),0(R2)                                                  
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFF                                                
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,(R3),DATAMGR                                   
         CLI   8(R1),0                                                          
         BE    GCPX                                                             
         DC    H'0'                                                             
*                                                                               
GCPX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       GETCLT                                        *         
***********************************************************************         
*                              INPUT: BAGYMD,BCLT                               
*                             OUTPUT: OFFICE # IN "CLTOFF"                      
*                             OUTPUT: CLT RATING SOURCE IN "CLTSRC"             
GETCLT   NTR1                                                                   
         LA    R4,KEY               BUILD THE KEY                               
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,X'00'                                                   
         MVC   CKEYAM,BAGYMD        AGENCY/MEDIA CODE                           
         MVC   CKEYCLT,BCLT         BINARY CLIENT CODE                          
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   CLTOFF,COFFICE      OFFICE NUMBER                                
         MVI   CLTSRC,C'N'         NSI                                          
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   CLTSRC,C'A'         ARB                                          
         DROP  R6                                                               
*                                                                               
GCLTX    B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       VALDLN                                        *         
***********************************************************************         
*                              INPUT: R2=A(FLD HDR OF DAYPART/LENGTH)           
*                             OUTPUT: DAYPART  IN "DPT"                         
*                                     DPT TIME SHEET PRT IN "QDPT"              
*                                     SPOT LEN IN "CMPSLN"                      
VALDLN   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ANY                 REQUIRED FIELD                               
*                                                                               
         CLI   COMDLNH+5,1                                                      
         BE    ERRMSLN             MISSING SPOT LENGTH ERROR                    
*                                                                               
         MVC   DPT,COMDLN          DAYPART                                      
         CLI   DPT,C'Z'                                                         
         BE    ERRDPT              DISALLOW DAYPART=Z                           
*                                                                               
         LA    R4,KEY              BUILD KEY OF DAYPART RECORD                  
         USING DPTHDRD,R4                                                       
         XC    DPTKEY,DPTKEY                                                    
         MVI   DPTKTYPE,8                                                       
         MVC   DPTKAGY,AGENCY                                                   
         MVC   DPTKMED,QMED                                                     
         MVC   DPTKMENU,ESTDMENU                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                THIS DAYPART RECORD MUST BE THERE            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DPTHDRD,R6                                                       
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ELEMENT                       
         USING DPTEL,R6                                                         
         LA    RE,DPTCODES         DAYPART CODES LIST                           
         DROP  R6                                                               
*                                                                               
VDLN10   CLI   0(RE),0             FIND THE DAYPART CODE                        
         BE    ERRDPT                                                           
         CLC   0(1,RE),DPT                                                      
         BE    *+12                FOUND - VALID DAYPART                        
         LA    RE,5(RE)                                                         
         B     VDLN10                                                           
*                                                                               
         MVC   QDPT,2(RE)                                                       
         XC    DPTSUBS,DPTSUBS                                                  
         MVI   DPTTYPE,C'R'        REGULAR DAYPART                              
         ZIC   R1,1(RE)                                                         
         SRL   R1,4                                                             
         LTR   R1,R1               TEST ANY SUB-DAYPARTS                        
         BZ    VDLN50              NO                                           
*                                                                               
         MVI   DPTTYPE,C'M'        MASTER DAYPART                               
         LA    R0,L'DPTSUBS                                                     
         LA    R5,DPTSUBS          YES - BUILD LIST OF SUB-DAYPARTS             
         USING DPTEL,R6                                                         
         LA    RE,DPTCODES         DAYPART CODES LIST                           
         DROP  R6                                                               
         SR    RF,RF                                                            
*                                                                               
VDLN20   CLI   0(RE),0                                                          
         BE    VDLN50                                                           
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1               TEST THIS IS A SUB-DAYPART                   
         BNE   VDLN40                                                           
         CLC   DPT,0(RE)           YES - TEST THIS IS OUR DAYPART               
         BNE   VDLN30                                                           
         CLI   DPTSUBS,0           YES - TEST ANY SUBDAYPARTS BEFORE            
         BE    VDLN40                                                           
         MVI   DPTTYPE,C'S'        YES - DPT IS NOT THE MASTER DPT,             
         XC    DPTSUBS,DPTSUBS           SO TYPE=SUB AND IT HAS                 
         B     VDLN50                    NO SUB-DAYPARTS                        
*                                                                               
VDLN30   MVC   0(1,R5),0(RE)                                                    
         LA    R5,1(R5)                                                         
         BCT   R0,VDLN40                                                        
         B     VDLN50                                                           
*                                                                               
VDLN40   LA    RE,5(RE)                                                         
         B     VDLN20                                                           
*                                                                               
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ERRDPT              YES-ERROR                                    
*                                                                               
VDLN50   ZIC   RE,COMDLNH+5                                                     
         BCTR  RE,0                MINUS 1 FOR DAYPART                          
*                                                                               
         CLI   COMDLN+1,C'/'                                                    
         BE    VDLN60                                                           
         CLI   COMDLN+1,C','                                                    
         BE    VDLN60                                                           
         B     *+6                                                              
*                                                                               
VDLN60   BCTR  RE,0                MINUS 1 FOR '/' OR ','                       
         LTR   RE,RE                                                            
         BZ    ERRMSLN             MISSING SPOT LEN                             
*                                                                               
         ZIC   R3,COMDLNH+5        INPUT LENGTH                                 
         LA    R5,COMDLN-1(R3)     LAST DIGIT IN DPT/SLN FIELD                  
VDLN70   CLI   0(R5),X'40'         IS THIS DIGIT SPACE/X'00'?                   
         BH    VDLN80              NO - NEXT                                    
         BCTR  R5,0                                                             
         BCT   RE,VDLN70                                                        
         B     ERRSLN                                                           
*                                                                               
VDLN80   LR    R4,RE               SAVE LENGTH OF SLN INPUT                     
*                                                                               
VDLN90   CLI   0(R5),C'0'          CHECK IF NUMERIC                             
         BL    ERRSLN                                                           
         CLI   0(R5),C'9'                                                       
         BH    ERRSLN                                                           
         BCTR  R5,0                                                             
         BCT   RE,VDLN90                                                        
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R5)                                                      
         CVB   RE,DUB                                                           
         STC   RE,SLN                                                           
*                                                                               
         LA    RE,SLNTAB                                                        
VDLN100  CLI   0(RE),0                                                          
         BE    ERRSLN                                                           
         CLC   SLN,0(RE)                                                        
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     VDLN100                                                          
         CLI   CMPSLN,0            VALID-TEST CAMPAIGN SLN IS NON-ZERO          
         BE    VDLNX                                                            
         CLC   CMPSLN,SLN          YES-SLN'S MUST MATCH                         
         BNE   ERRSLN                                                           
*                                                                               
VDLNX    CLC   XFRMDLN,COMDLN                                                   
         BNE   XIT                                                              
         MVC   XFRMDPT,DPT                                                      
         MVC   XFRMSLN,SLN                                                      
         B     XIT                                                              
SLNTAB   DS    0XL1                                                             
       ++INCLUDE SPSLNTAB                                                       
         DC    AL1(0)                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET UP THE KEY FIELDS THAT WERE STORED IN GLOBBER BY SPNWS14                  
***********************************************************************         
SETGLOB  NTR1  BASE=*,LABEL=*                                                   
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
*                                                                               
***      GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LNQ,GLVBUY1  1ST DATA ELEM         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM1LQ2,GLVBUY1  1ST DATA ELEM         
         CLI   8(R1),GLEGNF        NOT FOUND?                                   
         BE    STGLBX              LET IT VALIDATE THE STAR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         LA    R2,BLOCK                                                         
         USING NUKEM1D,R2                                                       
         MVC   COMMED,NUKM1MED                                                  
         MVI   COMMEDH+5,1                                                      
         OI    COMMEDH+1,X'20'                                                  
         OI    COMMEDH+6,X'80'                                                  
*                                                                               
         MVC   COMBYR,NUKM1BYR                                                  
         GOTOR CALCFLEN,DMCB,(L'COMBYR,COMBYRH)                                 
         OI    COMBYRH+1,X'20'                                                  
*                                                                               
         MVC   COMCAM,NUKM1CAM                                                  
         MVI   COMCAMH+5,5                                                      
         OI    COMCAMH+1,X'20'                                                  
         OI    COMCAMH+6,X'80'                                                  
*                                                                               
         MVC   COMSTA,NUKM1STA                                                  
         GOTOR CALCFLEN,DMCB,(L'COMSTA,COMSTAH)                                 
         OI    COMSTAH+1,X'20'                                                  
*                                                                               
         MVC   COMDAY,NUKM1DYS                                                  
         MVI   COMDAYH+5,7                                                      
         MVC   XFRMDAY,COMDAY      IN CASE THE USER CHANGES THIS                
****     OI    COMDAYH+1,X'20'                                                  
         OI    COMDAYH+6,X'80'                                                  
*                                                                               
         MVC   COMTIM,NUKM1TMS                                                  
         MVC   XFRMTIM,COMTIM      IN CASE THE USER CHANGES THIS                
         GOTOR CALCFLEN,DMCB,(L'COMTIM,COMTIMH)                                 
****     OI    COMTIMH+1,X'20'                                                  
*                                                                               
         MVC   COMDLN,NUKM1DSL                                                  
         MVC   XFRMDLN,COMDLN      IN CASE THE USER CHANGES THIS                
         GOTOR CALCFLEN,DMCB,(L'COMDLN,COMDLNH)                                 
****     OI    COMDLNH+1,X'20'                                                  
*                                                                               
         MVC   COMDEM,NUKM1DMO                                                  
         GOTOR CALCFLEN,DMCB,(L'COMDEM,COMDEMH)                                 
*                                                                               
         MVC   COMWKY,NUKM1WKY                                                  
         GOTOR CALCFLEN,DMCB,(L'COMWKY,COMWKYH)                                 
*                                                                               
         MVC   COMUPG,NUKM1UPG                                                  
         GOTOR CALCFLEN,DMCB,(L'COMUPG,COMUPGH)                                 
*                                                                               
         MVC   COMBK1,NUKM1BKS                                                  
         GOTOR CALCFLEN,DMCB,(L'COMBK1,COMBK1H)                                 
*                                                                               
         MVC   COMBK2,NUKM1BKS+1*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK2,COMBK2H)                                 
*                                                                               
         MVC   COMBK3,NUKM1BKS+2*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK3,COMBK3H)                                 
*                                                                               
         MVC   COMBK4,NUKM1BKS+3*L'NUKM1BKS                                     
         GOTOR CALCFLEN,DMCB,(L'COMBK4,COMBK4H)                                 
*                                                                               
         MVC   SVUDAY,NUKM1ODY     OVERRIDE DAY                                 
         MVC   SVUTIM,NUKM1OTM     OVERRIDE TIMES                               
*                                                                               
         MVC   SVUFILE,NUKM1UFL                                                 
         MVC   SVUGRD,NUKM1UEQ                                                  
         MVC   SVUFRBK,NUKM1FBK                                                 
         TM    NUKM1FBK+1,BTY2CHAR                                              
         BNO   STGLB10T                                                         
         CLI   NUKM1FBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUFRBT,NUKM1FBT                                                 
STGLB10T MVC   SVUFBL,NUKM1BLS                                                  
         MVC   SVUPUT,NUKM1UPT                                                  
         MVC   SVUSHR,NUKM1USH                                                  
         DROP  R2                                                               
*                                                                               
         L     R1,SYSPARMS         RF = A(GLOBBER)                              
         L     R1,16(R1)                                                        
         L     RF,CGLOBBER-COMFACSD(R1)                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,NUKM2LNQ,GLVBUY2  2ND DATA ELEM         
         CLI   8(R1),GLEGNF        NOT FOUND?                                   
         BE    STGLBX              LET IT VALIDATE THE STAR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T HANDLE OTHER ERRORS                    
*                                                                               
         LA    R2,BLOCK                                                         
         USING NUKEM2D,R2                                                       
         MVC   SVBWDPRG,NUKM2PRG                                                
         MVC   SVBWDBBY,NUKM2BBY                                                
         MVC   SVBWDSEQ,NUKM2SEQ   <==  SHOULD BE AT LEAST = X'01'              
         MVC   SVBWDRTG,NUKM2RTG                                                
         MVC   SVBWDFLG,NUKM2FLG                                                
         MVC   SVMLPMSD,NUKM2LPM                                                
         MVC   SVMALPHA,NUKM2ALF                                                
         DROP  R2                                                               
*                                                                               
STGLBX   B     XIT                 DONE WITH GLOBBER STUFF, VALIDATE            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       GETPUTS                                       *         
***********************************************************************         
GETPUTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    EBLOCK,EBLOCK       PREPARE EDITOR BLOCK                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'COMPT1                                                  
         XC    SPDEMLK,SPDEMLK     LOOK UP PUT VALUES FOR EACH BOOK             
***  NEED TO CLEAR OUT SPDEMLK EXTENDED BLOCK AS WELL                           
         XC    SVDEMLK,SVDEMLK                                                  
***  NEED TO CLEAR OUT SPDEMLK EXTENDED BLOCK AS WELL                           
         LA    RE,SVDEMLK                                                       
         ST    RE,SPLKXTND                                                      
         L     RE,AIO3                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAGY,AGENCY                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKMED,QMED                                                     
         MVI   SPLKFIL,C'T'                                                     
*****  CABLE/FUSION DATA LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    GETP00E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),QSTANEW+5   MOVE THE NETWORK IN                       
         USING SPLKXTD,RE                                                       
         L     RE,SPLKXTND         NEED TO PUT THE HEADEND IN EXTENDED          
         MVC   SPXTHEAD,QSTA                                                    
         B     GETP00G                                                          
         DROP  RE                                                               
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
*                                                                               
GETP00E  MVC   SPLKSTA,QSTA                                                     
GETP00G  MVC   SPLKSRC,CLTSRC                                                   
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    GETP00K                                                          
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
****  CABLE/FUSION TEMP CODE                                                    
         CLI   QSTA,C'0'                                                        
         BL    *+10                NOT CABLE, SKIP IT                           
****  CABLE/FUSION TEMP CODE                                                    
***      B     *+10                    SUPPRESS MARKET OVERRIDE                 
GETP00K  MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKDAY,DAY                                                      
         MVC   SPLKTIM,TIME                                                     
         MVI   SPLKSVI,X'FF'                                                    
*                                                                               
         CLI   SVWKY,C'W'          TEST 4 BOOKS, 1 WEEK                         
         BNE   *+14                                                             
         MVC   SPLKWKN,SVWKY+1     YES-SET WEEK NUMBER                          
         NI    SPLKWKN,X'0F'                                                    
*                                                                               
         LA    R1,G1WPROF                                                       
         ST    R1,SPLKA1W                                                       
         LA    R1,SVPUT                                                         
         ST    R1,SPLKALST                                                      
         LA    R1,WORK                                                          
         ST    R1,SPLKAVAL                                                      
         LA    R0,4                                                             
         LA    R2,COMPT1H                                                       
         LA    R4,SVPUTVAL                                                      
         LA    R5,SVBKS                                                         
         LA    R3,SVBKTPS          SVBKS BOOK TYPES                             
         LA    R6,1                                                             
         XC    SVPUTVAL,SVPUTVAL                                                
*                                                                               
GP10     XC    8(L'COMPT1,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   SPLKBTYP,STABKTYP                                                
         OC    0(2,R5),0(R5)                                                    
         BZ    GP20                                                             
*                                                                               
         MVC   SPLKDBK,0(R5)                                                    
*                                                                               
         TM    1(R5),BTYBITSQ      ANY SPECIAL BOOK TYPE?                       
         BZ    GP14                                                             
         TM    1(R5),BTY2CHAR      2 CHARACTER BOOKTYPE?                        
         BNO   GP13R                - NOPE                                      
         MVC   SPLKBTYP,0(R3)                                                   
         B     GP13T                                                            
*                                                                               
GP13R    GOTO1 =A(GTBKTYPE),DMCB,(C'B',1(R5)),SPLKBTYP,RR=RELO                  
GP13T    NI    SPLKDBK+1,X'FF'-BTYBITSQ                                         
         B     GP18                                                             
*                                                                               
GP14     CLI   STABKTYP,C'H'       NO, HISPANIC OR NO BOOK TYPE CAN             
         BE    *+12                   HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   GP18                                                             
*                                                                               
         OC    SVMLPMSD,SVMLPMSD   ANY LPM START DATE?                          
         BZ    GP18                NONE                                         
         GOTO1 DATCON,DMCB,(2,SVMLPMSD),(3,FULL)                                
*                                                                               
         CLC   SPLKDBK(2),FULL   ON OR AFTER LPM START DATE?                    
         BL    GP18                NO                                           
*                                                                               
         CLI   SPLKBTYP,C'H'       WAS ION OR EST HAS HISPANIC BOOK?            
         BNE   *+12                                                             
         MVI   SPLKBTYP,C'I'            SET ON HISPANIC PEOPLE METER            
         B     GP18                                                             
         MVI   SPLKBTYP,C'P'            SET ON PEOPLE METER                     
*                                                                               
GP18     TM    SVWKYIND,SVONEBK    TEST 1 BOOK, 4 WEEKS                         
         BZ    *+8                                                              
         STC   R6,SPLKWKN          YES-SET THE WEEK NUMBER                      
*                                                                               
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GP18E                - NOPE                                      
         OI    SPLKOPT,SPLKOP2D    - YUP, SPECIAL 2 DECIMAL LOOKUP              
***  2 DECIMAL  ***                                                             
GP18E    GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)     CALL SPGETDEM                  
         MVC   0(4,R4),WORK        PUT VALUE RETURNED                           
         ST    R4,EBAIN            FORMAT IT                                    
         LA    R1,8(R2)                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLC   0(4,R4),=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
GP20     LA    R2,COMPT2H-COMPT1H(R2)   NEXT BOOK                               
         LA    R4,4(R4)                                                         
         LA    R5,2(R5)                                                         
         LA    R3,1(R3)            BUMPING THE SVBKTPS                          
         LA    R6,1(R6)                                                         
         BCT   R0,GP10                                                          
*                                                                               
GPUTX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE RATING AND SHARE VALUES                          *         
*            AND FORMAT THE CUM SHARES                                *         
***********************************************************************         
*                       GETDEMS                                       *         
***********************************************************************         
GETDEMS  NTR1  BASE=*,LABEL=*                                                   
         XC    SPDEMLK,SPDEMLK                                                  
         XC    SVDEMLK,SVDEMLK     CLEARING OUT THE EXTENDED AREA               
         LA    RE,SVDEMLK                                                       
         ST    RE,SPLKXTND                                                      
         L     RE,AIO3                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         MVC   SPLKAGY,AGENCY                                                   
         MVC   SPLKMED,QMED                                                     
         MVC   SPLKCLI,QCLT                                                     
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    GETD00K                                                          
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT                                   
****  CABLE/FUSION TEMP CODE                                                    
         CLI   QSTA,C'0'                                                        
         BL    *+10                NOT CABLE, SKIP IT                           
****  CABLE/FUSION TEMP CODE                                                    
***      B     *+10                    SUPPRESS MARKET OVERRIDE                 
GETD00K  MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKDAY,DAY                                                      
         MVC   SPLKTIM,TIME                                                     
         MVI   SPLKSVI,X'FF'                                                    
*                                                                               
         CLI   SVWKY,C'W'          TEST 4 BOOKS, 1 WEEK                         
         BNE   *+14                                                             
         MVC   SPLKWKN,SVWKY+1     YES-SET WEEK NUMBER                          
         NI    SPLKWKN,X'0F'                                                    
*                                                                               
         LA    R1,G1WPROF                                                       
         ST    R1,SPLKA1W                                                       
*                                                                               
*        CLI   CUDMED,C'C'         TEST CANADA                                  
*        BNE   GETD1                                                            
*        TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
*        BZ    GETD1                                                            
*        XC    L1WPROF,L1WPROF     YES-PASS A(1W PROFILE)                       
*        LA    R1,L1WPROF                                                       
*        MVI   3(R1),C'Y'                                                       
*        ST    R1,SPLKA1W                                                       
*                                                                               
GETD1    LA    R1,SVRTGSHR                                                      
         ST    R1,SPLKALST                                                      
         LA    R1,WORK                                                          
         ST    R1,SPLKAVAL                                                      
         LA    R1,SVSTA            A(STATION LIST)                              
         ST    R1,ASVSTA                                                        
         LA    R1,SVPROGS          A(PROGRAM NAME LIST)                         
         ST    R1,ASVPROGS                                                      
         LA    R1,SVDEMVAL         A(DEMO VALUE AREA)                           
         ST    R1,ASVDEMVL                                                      
         L     RE,ASVPROGS         CLEAR PROGRAM NAME AREA                      
         LA    RF,SVPROGSL                                                      
         XCEFL                                                                  
         L     RE,ASVDEMVL         CLEAR DEMO VALUE AREA                        
         LA    RF,SVDEMVLL                                                      
         XCEFL                                                                  
         XC    SVCUMS,SVCUMS       CLEAR CUM SHARE VALUE AREA                   
         ZIC   R2,SVSTANO          R2 = NUMBER OF STATIONS                      
*                                                                               
GETD2    DS    0H                                                               
         L     RE,ASVSTA           STATION                                      
*****  CABLE/FUSION DATA LOOKUP                                                 
         CLI   0(RE),C'0'          IS IT A NUMBER?                              
         BL    GETD200E             - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),5(RE)    MOVE THE NETWORK IN                          
         USING SPLKXTD,RF                                                       
         L     RF,SPLKXTND         NEED TO PUT THE HEADEND IN EXTENDED          
***      XC    SVDEMLK,SVDEMLK     CLEARING OUT THE EXTENDED AREA               
***   IT IS CLEARED IN THE BEGINNING                                            
         MVC   SPXTHEAD,QSTA       USE THE ORIGINAL SYSCODE FOR NOW             
         B     GETD200G                                                         
         DROP  RF                                                               
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
GETD200E MVC   SPLKSTA,0(RE)       STATION                                      
         MVC   SPLKSPL,8(RE)       SPILL MARKET (IF ANY)                        
GETD200G LA    R0,4                                                             
         LA    R3,SVCUMS           R3 = A(CUM SHARE VALUE AREA)                 
         LA    R6,SVBKS            R6 = A(BOOK LIST)                            
         LA    R5,SVBKTPS          R5 = A(BOOKTYPE LIST)                        
         LA    RE,COMBK1H                                                       
         STCM  RE,15,DUB           SAVE THE SCREEN IN DUB                       
         LA    R4,1                                                             
*                                                                               
GETD3    MVC   SPLKBTYP,STABKTYP                                                
         OC    0(2,R6),0(R6)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R6)                                                    
         NI    SPLKOPT,X'FF'-SPLKOEXO                                           
*                                                                               
         TM    1(R6),BTYBITSQ      ANY SPECIAL BOOK TYPE?                       
         BZ    GETD3G                                                           
         TM    1(R6),BTY2CHAR      2 CHARACTER BOOKTYPE?                        
         BNO   GETD3C               - YUP                                       
         CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPLKBTYP,0(R5)                                                   
         B     GETD3E                                                           
*                                                                               
GETD3C   GOTO1 =A(GTBKTYPE),DMCB,(C'B',1(R6)),SPLKBTYP,RR=RELO                  
GETD3E   NI    SPLKDBK+1,X'FF'-BTYBITSQ                                         
         B     GETD4                                                            
*                                                                               
GETD3G   CLI   STABKTYP,C'H'       NO, HISPANIC OR NO BOOK TYPE CAN             
         BE    *+12                   HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   GETD4                                                            
*                                                                               
         OC    SVMLPMSD,SVMLPMSD   ANY LPM START DATE?                          
         BZ    GETD4               NONE                                         
         GOTO1 DATCON,DMCB,(2,SVMLPMSD),(3,FULL)                                
*                                                                               
         CLC   SPLKDBK(2),FULL     ON OR AFTER LPM START DATE?                  
         BL    GETD3I              NO                                           
*                                                                               
         ICM   RE,15,DUB           ADDRESS OF BOOK ON SCREEN                    
         CLI   14(RE),C'+'         IGNORE LPM?                                  
         BNE   GETD3K               - NOPE, WE NEED LPM                         
         B     GETD4                - YES, IGNORE LPM                           
*                                                                               
****   WE'RE BEFORE LPM START DATE!!!                                           
GETD3I   ICM   RE,15,DUB           ADDRESS OF BOOK ON SCREEN                    
         CLI   14(RE),C'+'         TRIED TO IGNORE LPM?                         
         BNE   GETD3K               - NOPE, DIDN'T TRY, NO LPM ANYWAY           
         MVI   14(RE),C' '         CLEAR OUT THE C'+'                           
         OI    6(RE),X'80'         RETRANSMIT                                   
         B     GETD4                - YES, TRIED TO IGNORE LPM                  
****   WE'RE BEFORE LPM START DATE!!!                                           
*                                                                               
GETD3K   CLI   SPLKBTYP,C'H'       WAS ION OR EST HAS HISPANIC BOOK?            
         BNE   *+12                                                             
         MVI   SPLKBTYP,C'I'            SET ON HISPANIC PEOPLE METER            
         B     GETD4                                                            
         MVI   SPLKBTYP,C'P'            SET ON PEOPLE METER                     
*                                                                               
GETD4    CLI   SPLKBTYP,C'O'                                                    
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOEXO    YES                                          
*                                                                               
         TM    SVWKYIND,SVONEBK    TEST 1 BOOK, 4 WEEKS                         
         BZ    *+8                                                              
         STC   R4,SPLKWKN          YES-SET THE WEEK NUMBER                      
*                                                                               
         XC    SPLKPRG,SPLKPRG     CLEAR PREV PROGRAM NAME                      
***  2 DECIMAL  ***                                                             
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GETD4E               - NOPE                                      
         OI    SPLKOPT,SPLKOP2D    - YUP, SPECIAL 2 DECIMAL LOOKUP              
***  2 DECIMAL  ***                                                             
GETD4E   GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLK)     CALL SPGETDEM                  
*                                                                               
         L     RE,ASVPROGS                                                      
         MVC   0(L'L1PROG1,RE),SPLKPRG    PROGRAM NAME                          
         L     RE,ASVDEMVL                                                      
         MVC   0(4,RE),WORK        RATING                                       
         MVC   4(4,RE),WORK+8      SHARE                                        
         L     R1,0(R3)            ACCUMULATE TOTAL BOOK SHARE                  
         A     R1,WORK+8                                                        
         ST    R1,0(R3)                                                         
*                                                                               
GETD6    LA    R3,4(R3)            NEXT CUM SHARE AREA                          
         L     RE,ASVPROGS                                                      
         LA    RE,L'L1PROG1(RE)                                                 
         ST    RE,ASVPROGS                                                      
         XR    RE,RE               LET'S CLEAR OUT RE                           
         ICM   RE,15,DUB           MOVE TO THE NEXT BOOK ON SCREEN              
         XR    R1,R1               LET'S CLEAR OUT R1                           
         ICM   R1,1,0(RE)                                                       
         AR    RE,R1                                                            
         ICM   R1,1,0(RE)          NEED TO DO THIS AGAIN                        
         AR    RE,R1                                                            
         STCM  RE,15,DUB                                                        
         LA    R6,2(R6)            NEXT BOOK                                    
         LA    R5,1(R5)            NEXT BOOK TYPE                               
         L     RE,ASVDEMVL                                                      
         LA    RE,8(RE)                                                         
         ST    RE,ASVDEMVL                                                      
         LA    R4,1(R4)                                                         
         BCT   R0,GETD3                                                         
*                                                                               
         L     RE,ASVSTA                                                        
         LA    RE,L'SVSTA(RE)      NEXT STATION                                 
         ST    RE,ASVSTA                                                        
         BCT   R2,GETD2                                                         
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT CUM SHARES                            
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,L'CUM1                                                    
         MVI   EBDECS,1                                                         
         OI    EBOPT,X'20'         ZERO=NOBLANK                                 
         LA    R0,4                4 CUM SHARE COLUMNS                          
         LA    R3,SVCUMS           R3 = A(1ST CUM SHARE BINARY)                 
         LA    R4,COMCUM                                                        
         USING CUMLINED,R4                                                      
         LA    R6,CUM1             R6 = A(1ST CUM SHARE FIELD)                  
         LA    R5,SVBKS            R5 = A(1ST BOOK)                             
***      LA    R2,SVBKTPS          R2 = A(1ST BOOKTYPE)                         
*                                                                               
GETD8    OC    0(2,R5),0(R5)       ANY BOOK HERE?                               
         BNZ   *+14                YES                                          
         XC    0(L'CUM1,R6),0(R6)  NO, CLEAR THIS COLUMN'S CUM SHARE            
         B     GETD9                                                            
*                                                                               
         ST    R3,EBAIN                                                         
         ST    R6,EBAOUT                                                        
         GOTO1 EDITOR,DMCB,EBLOCK                                               
GETD9    LA    R5,2(R5)                                                         
         LA    R3,4(R3)                                                         
         LA    R6,CUM2-CUM1(R6)                                                 
         BCT   R0,GETD8                                                         
         OI    COMCUMH+6,X'80'                                                  
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       GETPJDOV                                      *         
***********************************************************************         
GETPJDOV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVCMPKEY),SVCMPKEY                                         
*                                                                               
         GOTO1 HIGH                CHECK IF COMPETITION REC EXIST?              
         CLC   KEY(L'SVCMPKEY),KEYSAVE                                          
         BNE   GOV500              NO - EXIT                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CMSCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   GOV500              NO STATION LIST ELEMENT - EXIT               
*                                                                               
         USING CMSEL,R6                                                         
         LA    R2,CMSSTA           STATION LIST ADDRESS                         
         LA    R4,CMSSEND          STATION LIST END ADDR                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CMDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   GOV200              NO DEMO VALUE ELEM - DONE W/ CMP REC         
*                                                                               
         USING CMDEL,R6                                                         
GOV10    CLC   CMDTYP,SVDEM                                                     
         BE    GOV20               FOUND                                        
         BAS   RE,NEXTEL                                                        
         BNE   GOV100              THIS DEMO ELEM NOT FOUND - EXIT              
         B     GOV10                                                            
*                                                                               
GOV20    LA    R3,CMDDEMO          DEMO VALUES LIST ADDRESS                     
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CMDLEN                                                        
         LA    RF,CMDEL(RF)        POINTS TO THE END OF CMDDEMO                 
         STCM  RF,15,CMDDMEND      SAVE OFF FOR COMPARE LATER                   
*                                                                               
         MVC   SVPJPUT,CMDPUT      OVERRIDE THE PJ PUT                          
         DROP  R6                                                               
*                                  RE-DISPLAY THE PJ PUT                        
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVC   FULL,SVPJPUT                                                     
         LA    RF,FULL                                                          
         NI    0(RF),255-X'80'                                                  
         LA    R1,FULL                                                          
         ST    R1,EBAIN                                                         
         MVI   EBLOUT,L'COMPPT                                                  
         LA    R1,COMPPT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         TM    SVPJPUT,X'80'                                                    
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         CLC   FULL,=F'1000'                                                    
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 EDITOR,DMCB,EBLOCK                                               
         OI    COMPPTH+6,X'80'                                                  
         OI    COMPPTH+4,X'20'                                                  
*                                                                               
         LA    R5,SVSTA                                                         
         LA    R6,SVPROJ                                                        
         ZIC   R0,SVSTANO                                                       
*                                                                               
GOV30    LR    RE,R2               A(STATION LIST) IN COMP REC                  
         LR    RF,R3               A(DEMO VALUES LIST) IN COMP REC              
*                                                                               
GOV40    CR    RE,R4               PASSED THE END OF STATION LIST               
         BNL   GOV70                                                            
         CLI   0(RE),0             NO MORE STATION                              
         BE    GOV70                                                            
*                                                                               
         CLM   RF,15,CMDDMEND                                                   
         BNL   GOV62               NO MORE DEMO VALUES, DON'T SAVE              
         CLC   0(L'CMSSTA,RE),0(R5)                                             
         BE    GOV60               FOUND                                        
         LA    RE,L'CMSSTA(RE)     NEXT STATION IN COMP REC                     
         LA    RF,L'CMDDEMO(RF)    NEXT RATING+SHARE VALUE IN COMP REC          
         B     GOV40                                                            
*                                                                               
GOV60    MVC   0(8,R6),1(RF)       RATING + SHARE                               
GOV62    CLC   0(L'CMSSTA,RE),SVSTA  PRIMARY STATION?                           
         BE    GOV70                 YES, LEAVE IT ALONE                        
         GOTO1 =A(GETFRNWS),RR=RELO                                             
*                                                                               
GOV70    LA    R5,L'SVSTA(R5)                                                   
         LA    R6,8(R6)                                                         
         BCT   R0,GOV30            NEXT STATION                                 
*                                                                               
*                                  RE-CALCULATE ALL RATINGS                     
         ZIC   R6,SVSTANO          # STATIONS                                   
         LA    R3,SVPROJ           SAVED PJ DEMO VALUES LIST                    
         LA    R4,OLDPROJ          ORIGINAL PJ DEMO VALUES LIST                 
****  HMMM DO WE EVEN NEED THIS?!?                                              
*        BRAS  RE,ADJPREC          GONNA CONVERT SVPROJ                         
****                                                                            
         L     RE,SVPJPUT          PUT                                          
         SLL   RE,1                ELIMINATE OVERRIDE BIT                       
         SRL   RE,1                                                             
*                                                                               
GOV80    SR    R1,R1               PREPARE FOR DIVIDE                           
         L     R0,4(R3)            SHARE                                        
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
         SRDL  R0,32                                                            
         MR    R0,RE                                                            
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GOV80E               - NOPE, NORMAL                              
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GOV80E              IT IS IMPRESSION                             
         D     R0,=F'100'                                                       
         B     GOV80G                                                           
***  2 DECIMAL                                                                  
GOV80E   D     R0,=F'1000'                                                      
GOV80G   LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R3)                                                         
*                                                                               
         CLC   0(4,R3),0(R4)       COMPARE THE ORIGINAL DEMO RATING             
         BE    *+8                                                              
         OI    0(R3),X'80'         RATING OVERRIDE BIT                          
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    GOV80K               - NOPE, NORMAL                              
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GOV80K              IT IS IMPRESSION                             
         OI    0(R3),X'40'                                                      
***  2 DECIMAL                                                                  
*                                                                               
GOV80K   L     R1,4(R3)            SHARE                                        
         SLL   R1,1                ELIMINATE OVERRIDE BIT                       
         SRL   R1,1                                                             
         C     R1,4(R4)            COMPARE THE ORIGINAL DEMO RATING             
         BE    *+8                                                              
         OI    4(R3),X'80'         RATING OVERRIDE BIT                          
*                                                                               
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,GOV80                                                         
*                                                                               
GOV100   L     R6,AIO                                                           
         LA    R2,CMSSTA-CMPREC(R6)                                             
         MVI   ELCODE,CMPCODEQ     PROGRAM NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   GOV200                                                           
*                                                                               
         ST    R6,FULL             1ST PROG NAME ELEMENT ADDR                   
*                                                                               
         LA    R5,SVSTA                                                         
         LA    R3,SVPJPRGS                                                      
         ZIC   R0,SVSTANO                                                       
*                                                                               
GOV110   LR    RE,R2               A(STATION LIST) IN COMP REC                  
         L     R1,FULL                                                          
*                                                                               
GOV120   CR    RE,R4               PASSED THE END OF STATION LIST               
         BNL   GOV140                                                           
         CLI   0(R2),0             NO MORE STATION                              
         BE    GOV140                                                           
*                                                                               
         CLC   0(L'CMSSTA,RE),0(R5)                                             
         BE    GOV130              FOUND                                        
         LA    RE,L'CMSSTA(RE)     NEXT STATION IN COMP REC                     
         AHI   R1,L'CMPCODE+L'CMPLEN+L'CMPSEQ+L'CMPPROG  NEXT PRG ELEM          
         B     GOV120                                                           
*                                                                               
GOV130   TM    L'L1PJPROG(R3),X'80'   OVERWROTE THE PRG NAME ALREADY?           
         BNZ   GOV140                 YES, FROM 1ST DUPLICATE                   
         MVC   0(L'L1PJPROG,R3),CMPPROG-CMPEL(R1)   PROG NAME                   
*                                                                               
GOV140   LA    R5,L'SVSTA(R5)                                                   
         LA    R3,L'SVPJPRGS(R3)                                                
         BCT   R0,GOV110           NEXT STATION                                 
*                                                                               
GOV200   DS    0H                                                               
*                                                                               
GOV500   CLI   XFRCALL,C'Y'        COMING FROM GLOBBER?                         
         BNE   GOVX                                                             
         MVC   SVPJPRGS,SVBWDPRG                                                
*                                                                               
GOVX     B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GONNA ADJUST THE DEMO VALUE PRECISION FROM 1 TO 2                             
* INPUT: (R3)  ADDRESS OF DEMO VALUE(S) THAT NEEDS TO BE CHANGED                
*              SUCH AS: SVPROJ                                                  
**********************************************************************          
ADJPREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,SVPROJ                                                        
         AHI   RF,SVPROJL          RF NOW POINTS TO END OF SVPROJ               
*                                                                               
         CLI   SVDEM+1,C'R'        IS IT R OR E?                                
         BE    ADJPRC00                                                         
         CLI   SVDEM+1,C'E'                                                     
         BNE   ADJPRECX                                                         
*                                                                               
ADJPRC00 CR    R3,RF               DID WE REACH THE END YET?                    
         BNL   ADJPRECX                                                         
*                                                                               
ADJPRC10 MVC   FULL(1),0(R3)                                                    
*                                                                               
         TM    FULL,X'40'          DEMO HAS 2 DEC?                              
         BO    ADJPRC20            YES                                          
*                                                                               
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    ADJPCBMP             - NO, JUST EXIT                             
*                                                                               
         NI    0(R3),X'3F'         YES, DROP FLAGS FROM VALUE                   
         ICM   R0,15,0(R3)         ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         STCM  R0,15,0(R3)                                                      
         OI    0(R3),X'40'         SET 2 DEC PRECISION FLAG                     
         B     ADJPRC22                                                         
*=========================================================                      
* DEMO HAS 2 DEC                                                                
*=========================================================                      
ADJPRC20 DS    0H                                                               
*                                                                               
         TM    BITFLAG1,BF1TWODC   IS 2 DECIMAL PRECISION ON?                   
         BO    ADJPCBMP             - YUP, DONE!                                
*                                                                               
         XR    R0,R0               CLEAR OUT IN CASE IT HAS SOMETHIN'           
         NI    0(R3),X'3F'         DROP FLAGS FROM VALUE                        
         ICM   R1,15,0(R3)         ADJUST 2 DECIMAL PRECISION TO 1              
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STCM  R1,15,0(R3)                                                      
*                                                                               
ADJPRC22 TM    FULL,X'80'          TEST OVERRIDE FLAG WAS SET                   
         BZ    *+8                                                              
         OI    0(R3),X'80'         SET IT BACK ON                               
*                                                                               
ADJPCBMP LA    R3,8(R3)            BUMP TO NEXT DEMO R/S VALUES                 
         B     ADJPRC00             - NEXT!                                     
*                                                                               
ADJPRECX J     XIT                                                              
***********************************************************************         
* THIS ROUTINE READS THE NWS RECORDS TO PULL THE 1ST RECORD IN CASE             
* THERE ARE DUPLICATES                                                          
*                                                                               
* ON ENTRY:    (R5)                STATION WE'RE UPTO                           
*              (R6)                RATING/SHARE WE'RE UPTO                      
***********************************************************************         
GETFRNWS NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BWHKEY,R4                                                        
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM        THIS IS ALREADY ONES COMPLEMENTED            
         MVC   BWHKMKT,BMKT                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(BWHKSEQ-BWHKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BCMMKSEQ,BWHKSEQ    SAVE THE CAMP/MKT SEQ                        
         GOTO1 GETREC                                                           
***************                                                                 
* SCAN THRU ALL THE NWS HEADER'S STATION ELEMENTS                               
***************                                                                 
         L     R4,AIO                                                           
         LA    R4,BWHFSTEL                                                      
         NI    BITFLAG1,X'FF'-BF1BUYRV                                          
         XR    R0,R0                                                            
         MVI   NWSHBSTA,0          DIDN'T FIND STATION YET                      
GFNWS10  CLI   0(R4),0                                                          
         BE    GFNWSX                                                           
         CLI   0(R4),BWHELCDQ      STATION ELEMENT (X'02')?                     
         BE    GFNWS16                                                          
GFNWS13  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GFNWS10                                                          
*                                                                               
         USING BWHEL,R4                                                         
GFNWS16  DS    0H                                                               
         MVC   WORK(L'SVSTA),0(R5)                                              
         OI    WORK+4,C'T'         USE T INSTEAD OF BLANK                       
         CLC   BWHSTA,WORK         STATION MATCH?                               
***      CLC   BWHSTA,0(R5)        STATION MATCH?                               
         BNE   GFNWS13                                                          
         MVC   NWSHBSTA,BWHSEQ                                                  
***************                                                                 
* CONTINUE SCAN FOR THE INFO ELEM IF ANY                                        
***************                                                                 
GFNWS20  CLI   0(R4),0                                                          
         BE    GFNWS30                                                          
         CLI   0(R4),INFELCDQ      INFO ELEMENT (X'06')?                        
         BE    GFNWS25                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GFNWS20                                                          
*                                                                               
         USING INFELD,R4                                                        
GFNWS25  TM    INFFLAG1,IFF1BYRV   THIS CAMP/MKT DOING BUY REVISIONS?           
         BZ    *+8                                                              
         OI    BITFLAG1,BF1BUYRV   YES, DON'T HAVE TO CHANGE WORK RECS          
         DROP  R4                                                               
*                                                                               
GFNWS30  TM    BITFLAG1,BF1BUYRV   CAMP/MKT DOING BUY REVISIONS?                
         BNZ   GFNWS100                                                         
         TM    SVBWDFLG,NKMFBUYR   NO, BUT DID WE COME FROM BUY/SKED?           
         BNZ   GFNWSX                  YES, THEN DON'T UPDATE WORK RECS         
***********************************                                             
* FOR ALL NWSHBSTA, GOT THRU ALL NWS DETAIL RECORDS THAT HAVE THE SAME          
* DAYS/TIMES AND THEN USE THE RATINGS WE FIND FOR THE DEMO THAT IS USED         
***********************************                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BWDKEY,R4                                                        
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BCMMKSEQ                                                 
         MVI   BWDKELCD,BWDELCDQ   MINIO GROUPS STARTS WITH X'01'               
         MVC   BWDKELST,NWSHBSTA                                                
         MVC   BWDKELDY,XFRMBDAY                                                
*                                                                               
         GOTO1 HIGH                                                             
GFNWS46  CLC   KEY(BWDKEL-BWDKEY),KEYSAVE                                       
         BNE   GFNWSX              HAS TO MATCH BYTES TO & INCL SEQ             
***************                                                                 
* GOT A NWS DETAIL RECORD.  THERE MIGHT BE PKG/ORB DETAILS.                     
*                                                                               
* NOTE:  IF BWDKELST=NWSHBSTA THEN THERE CAN POTENTIALLY BE MORE FOR            
*         THE STATION (MINIO SPLIT ALGORITHM)                                   
***************                                                                 
         GOTO1 GETREC              IF BWDKELST=NTBLBNWS THEN THERE CAN          
         L     R4,AIO                  POTENTIALLY BE MORE FOR THE STA          
         LA    R4,BWDFSTEL         FIRST ELEMENT                                
GFNWS50  CLI   0(R4),0             END OF THIS RECORD                           
         BE    GFNWSX                                                           
         CLI   0(R4),BWDELCDQ      NEED TO FIND X'01' ELEM                      
         BE    GFNWS60                                                          
GFNWS55  XR    R0,R0               NEXT DETAIL RECORD                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GFNWS50                                                          
*                                                                               
         USING BWDEL,R4                                                         
GFNWS60  CLC   BWDSTACD,NWSHBSTA   NEED TO MATCH STATION WE'RE AT               
         BL    GFNWS55                                                          
         BH    GFNWSX              NO MORE FOR THE STATION WE'RE AT             
*                                                                               
         CLC   BWDDAYS,XFRMBDAY                                                 
         BNE   GFNWS55                                                          
         CLC   BWDTIMES,XFRMBTIM                                                
         BNE   GFNWS55                                                          
         LR    RE,R5                                                            
         LA    RF,SVSTA                                                         
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         SRP   DUB(8),64-1,0       DIVIDE BY 10 (L'SVSTA)                       
         CVB   RE,DUB                                                           
         MHI   RE,L'SVPJPRGS                                                    
         LA    RE,SVPJPRGS(RE)                                                  
         MVC   0(L'L1PJPROG,RE),BWDPROG                                         
         OI    L'L1PJPROG(RE),X'80'   REPLACED PROGRAM, DON'T OVERWRITE         
*                                                                               
         XR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),DMOELCDQ      NEED TO LOOK AT DEMO ELEM                    
         BNE   GFNWSX                                                           
         USING DMOEL,R4                                                         
         XR    R2,R2                                                            
         IC    R2,1(R4)                                                         
         AR    R2,R4               R2=A(BYTE AFTER END OF DEMO ELEM)            
         LA    R3,DMODEMO                                                       
GFNWS65  CLC   SVDEM(3),0(R3)      MATCH THE DEMO CATEGORY?                     
         BE    GFNWS70                                                          
         LA    R3,L'DMODEMO(R3)                                                 
         CR    R3,R2                                                            
         BL    GFNWS65                                                          
         B     GFNWSX              WE DON'T HAVE THIS DEMO                      
*                                                                               
GFNWS70  MVC   0(4,R6),4(R3)       REPLACE THE RATING W/ WHAT WE HAVE           
         B     GFNWS150            NEED TO CALCULATE THE SHARE                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************                                             
* FOR ALL ENTRIES IN ANWSTBL, GOT THRU ALL NWS REVISION RECORDS THAT            
* HAVE THE SAME DAYS/TIMES AND THEN MODIFY THE RATINGS TO MATCH THE             
* RATING FOR THE DEMO THAT WAS CHANGED.                                         
***********************************                                             
GFNWS100 TM    SVBWDFLG,NKMFBUYR   CAME FROM BUY/SKED?                          
         BZ    GFNWSX              NO, DON'T UPDATE BUY REVISION RECS           
*                                                                               
         GOTO1 MSPACK,DMCB,QMKT,0(R5),WORK                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NBRKEY,R4                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMMKSEQ                                                 
         MVC   NBRKSTA,WORK+2                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                  KEY MATCHES UP TO AND INCL STATION?          
GFNWS120 CLC   KEY(NBRKKBUY-NBRKEY),KEYSAVE                                     
         BNE   GFNWSX              NO, NEXT STATION THEN                        
*                                                                               
GFNWS125 GOTO1 GETREC                                                           
         NI    BITFLAG1,X'FF'-BF1PUTRC   DON'T NEED TO PUTREC YET               
         L     R4,AIO                                                           
         LA    R4,NBRFSTEL                                                      
         USING NBRSELD,R4                                                       
         CLC   NBRSDAYS,XFRMBDAY                                                
         BNE   *+14                                                             
         CLC   NBRSTIMS(4),XFRMBTIM                                             
         BE    GFNWS132                                                         
         GOTO1 SEQ                                                              
         B     GFNWS120                                                         
*                                                                               
         LR    RE,R5                                                            
         LA    RF,SVSTA                                                         
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         SRP   DUB(8),64-1,0       DIVIDE BY 10 (L'SVSTA)                       
         CVB   RE,DUB                                                           
         MHI   RE,L'SVPJPRGS                                                    
         LA    RE,SVPJPRGS(RE)                                                  
         MVC   0(L'L1PJPROG,RE),NBRSPROG                                        
         OI    L'L1PJPROG(RE),X'80'   REPLACED PROGRAM, DON'T OVERWRITE         
*                                                                               
GFNWS132 XR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GFNWSX                                                           
         CLI   0(R4),NBRDMELQ      DEMO ELEM (X'20')?                           
         BNE   GFNWS132                                                         
         USING NBRDMELD,R4                                                      
         XR    R2,R2                                                            
         IC    R2,1(R4)                                                         
         AR    R2,R4               R2=A(BYTE AFTER END OF DEMO ELEM)            
         LA    R3,NBRDMDMO                                                      
GFNWS134 CLC   SVDEM(3),0(R3)      MATCH THE DEMO CATEGORY?                     
         BE    GFNWS136                                                         
         LA    R3,L'DMODEMO(R3)                                                 
         CR    R3,R2                                                            
         BL    GFNWS134                                                         
         B     GFNWSX              WE DON'T HAVE THIS DEMO                      
*                                                                               
GFNWS136 MVC   0(4,R6),4(R3)       REPLACE THE RATING W/ WHAT WE HAVE           
*                                                                               
GFNWS150 SR    R1,R1               RE-CALCULATE SHARE                           
         ICM   R0,15,0(R6)         RATING                                       
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   WE DOING 2 DECIMAL?                          
         BZ    GFNWS180             - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GFNWS180            IT IS IMPRESSION                             
         SLL   R0,1                ELIMINATE 2 DECIMAL BIT                      
         SRL   R0,1                                                             
***  2 DECIMAL                                                                  
GFNWS180 SRDL  R0,32                                                            
***  2 DECIMAL                                                                  
         TM    BITFLAG1,BF1TWODC   WE DOING 2 DECIMAL?                          
         BZ    GFNWS185             - NOPE                                      
         CLI   SVDEM+1,C'R'                                                     
         BE    *+12                                                             
         CLI   SVDEM+1,C'E'                                                     
         BNE   GFNWS185            IT IS IMPRESSION                             
         M     R0,=F'100'                                                       
         B     GFNWS190                                                         
***  2 DECIMAL                                                                  
GFNWS185 SLL   RE,1                                                             
GFNWS190 SRL   RE,1                                                             
*                                                                               
         LTR   RE,RE                                                            
         BNZ   *+10                                                             
         XR    R1,R1                                                            
         B     GFNWS200                                                         
*                                                                               
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
GFNWS200 STCM  R1,15,4(R6)                                                      
         TM    0(R6),X'80'                                                      
         BZ    *+8                                                              
         OI    4(R6),X'80'                                                      
*                                                                               
GFNWSX   MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVCMPKEY),SVCMPKEY                                         
         GOTO1 HIGH                RESTORE KEY  SEQUENCE                        
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       GETRATOV                                      *         
***********************************************************************         
GETRATOV NTR1  BASE=*,LABEL=*      GET RATING OVERRIDE FROM WORK REC            
*                                                                               
         TM    BITFLAG1,BF1XFRCN   CAME FROM GLOBBER?                           
         BNZ   *+12                YES, DON'T CARE IF OVERRIDE OR NOT           
         TM    SVBWDRTG,X'80'      OVERRIDE?                                    
         BZ    GRO99               NO, THEN NOTHING HERE TO DO                  
*                                                                               
         MVC   SVPROJ(4),SVBWDRTG  OVERRIDE ONLY THE 1ST RATING                 
GRO10    SR    R1,R1               RE-CALCULATE SHARE                           
         ICM   R0,15,SVPROJ        RATING                                       
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
***  2 DECIMAL                                                                  
         TM    SVPROJ,DMODEM2D     WE DOING 2 DECIMAL?                          
         BZ    GRD10C               - NOPE, NORMAL                              
         SLL   R0,1                ELIMINATE 2 DECIMAL BIT                      
         SRL   R0,1                SVPROJ STILL HAS THE BIT ON                  
***  2 DECIMAL                                                                  
GRD10C   SRDL  R0,32                                                            
***  2 DECIMAL                                                                  
         TM    SVPROJ,DMODEM2D     WE DOING 2 DECIMAL?                          
         BZ    GRD10E               - NOPE, NORMAL                              
         M     R0,=F'100'                                                       
         B     GRD10G                                                           
***  2 DECIMAL                                                                  
*                                                                               
GRD10E   M     R0,=F'1000'                                                      
GRD10G   L     RE,SVPJPUT                                                       
         SLL   RE,1                                                             
         SRL   RE,1                                                             
*                                                                               
         LTR   RE,RE                                                            
         BNZ   *+10                                                             
         XR    R1,R1                                                            
         B     GRO20                                                            
*                                                                               
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
GRO20    STCM  R1,15,SVPROJ+4                                                   
*                                                                               
         CLC   SVPROJ+4(4),OLDPROJ+4 COMPARE TO THE ORIGINAL VALUE              
         BE    *+8                                                              
         OI    SVPROJ+4,X'80'         OVERRIDE BIT                              
*                                                                               
***GRO99    XC    SVBWDRTG,SVBWDRTG                                             
GRO99    DS    0H                                                               
*                                                                               
GROX     B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       GETPJCUM                                      *         
***********************************************************************         
GETPJCUM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RE,RE                                                            
         LA    R1,SVPROJ                                                        
         ZIC   R0,SVSTANO                                                       
*                                                                               
GPJC10   L     RF,4(R1)            LOAD SHARE                                   
         SLL   RF,1                GET RID OF OVERRIDE BIT                      
         SRL   RF,1                                                             
         AR    RE,RF                                                            
         LA    R1,8(R1)                                                         
         BCT   R0,GPJC10                                                        
*                                                                               
         ST    RE,SVPJCUM                                                       
         BRAS  RE,FMTPJCUM         FORMAT PROJ CUM SHARE                        
*                                                                               
GPJCX    B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       WPJDEMS                                       *         
***********************************************************************         
WPJDEMS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVCMPKEY),SVCMPKEY                                         
*                                                                               
         GOTO1 HIGH                CHECK IF COMPETITION REC EXIST?              
         CLC   KEY(L'SVCMPKEY),KEYSAVE                                          
         BNE   WPJD200             NO - CREATE A COMP REC                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CMSCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    WPJD05                                                           
*                                                                               
         OI    LCHG,LCNST                                                       
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMSEL,R6            BUILD THE STATION LIST ELEMENT               
         MVI   CMSCODE,CMSCODEQ                                                 
         MVI   CMSLEN,21*L'CMSSTA+L'CMSLEN+L'CMSCODE                            
*                                                                               
         LA    R1,SVSTA                                                         
         LA    R2,CMSSTA                                                        
         ZIC   R3,SVSTANO                                                       
*                                                                               
         MVC   0(L'CMSSTA,R2),0(R1)                                             
         LA    R1,L'SVSTA(R1)                                                   
         LA    R2,L'CMSSTA(R2)                                                  
         BCT   R3,*-14                                                          
*                                                                               
         GOTO1 ADDELEM                                                          
         B     WPJD45                                                           
*                                                                               
WPJD05   L     R6,AIO                                                           
         LA    R6,CMSEL-CMPREC(R6)                                              
         USING CMSEL,R6                                                         
*                                  EDIT STATION ELEM                            
         LA    R1,SVSTA                                                         
         ZIC   R3,SVSTANO                                                       
         LA    R4,CMSSEND                                                       
*                                                                               
WPJD10   LA    R2,CMSSTA                                                        
WPJD20   CR    R2,R4               PASSED THE END OF LIST                       
         BNL   WPJD25                                                           
*                                                                               
         CLC   0(L'CMSSTA,R2),0(R1)                                             
         BE    WPJD40               FOUND                                       
         LA    R2,L'CMSSTA(R2)                                                  
         B     WPJD20                                                           
*                                                                               
WPJD25   LA    RE,CMSSEND-L'CMSSTA                                              
         CLI   0(RE),0                                                          
***      BE    *+8                                                              
***      DC    H'0'                CAN'T ADD ANY MORE STATION                   
         BNE   WPJD45              CONTINUE WITH THE FIRST 20 STATIONS          
*                                                                               
         LA    RE,CMSSTA                                                        
WPJD30   CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,L'CMSSTA(RE)                                                  
         B     WPJD30                                                           
*                                  ADD A NEW STATION                            
         MVC   0(L'CMSSTA,RE),0(R1)                                             
         OI    LCHG,LCNST                                                       
*                                                                               
WPJD40   LA    R1,L'SVSTA(R1)                                                   
         BCT   R3,WPJD10                                                        
         DROP  R6                                                               
*                                                                               
WPJD45   TM    LCHG,LCNST+LCPGS    NEED TO UPDATE PROG ELEMENTS                 
         BZ    WPJD80                                                           
*                                                                               
*                                  EDIT PROGRAM ELEM                            
         L     R6,AIO                                                           
         USING CMPREC,R6                                                        
         LA    R2,CMSSTA                                                        
         LA    R4,CMSSEND                                                       
         LA    R3,1                STATION SEQ #                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,CMPCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         SR    R6,R6               INDICIATE THERE IS NO PROG ELEMENT           
*                                  R6 - A(1ST PROG ELEMENT)                     
*                                                                               
WPJD50   CR    R2,R4               PASSED THE END OF STATION LIST               
         BNL   WPJD80                                                           
         CLI   0(R2),0             NO MORE STATION                              
         BE    WPJD80                                                           
*                                                                               
         ZIC   R0,SVSTANO                                                       
         LA    R1,SVSTA                                                         
         LA    R5,SVPJPRGS                                                      
*                                  SEARCH STATION ON THE SVSTA LIST             
WPJD55   CLC   0(L'CMSSTA,R2),0(R1)                                             
         BE    WPJD57              FOUND                                        
         LA    R1,L'SVSTA(R1)                                                   
         LA    R5,L'SVPJPRGS(R5)   NEXT PROGRAM NAME                            
         BCT   R0,WPJD55                                                        
         B     WPJD70                                                           
*                                                                               
WPJD57   LTR   RE,R6               ANY PROGRAMS                                 
         BZ    WPJD60              ADD PROGRAM ELEM UNCONDITIONALLY             
*                                                                               
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'CMPCODE+L'CMPLEN+L'CMPSEQ+L'CMPPROG                         
         AR    RF,RE               DISPLACEMENT TO TARGET PROG ELEM             
*                                                                               
         USING CMPEL,RF                                                         
         CLI   CMPCODE,CMPCODEQ    PROG ELEM?                                   
         BNE   WPJD60              NO - ADD PROG ELEM                           
         STC   R3,BYTE                                                          
         CLC   CMPSEQ,BYTE         SAME SEQ #?                                  
         BNE   WPJD60              NO - ADD PROG ELEM                           
*                                                                               
         MVC   CMPPROG,0(R5)       CHANGE PROG ELEM                             
         OC    CMPPROG,SPACES                                                   
         B     WPJD70                                                           
         DROP  RF                                                               
*                                                                               
WPJD60   XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING CMPEL,RE                                                         
         MVI   CMPCODE,CMPCODEQ                                                 
         MVI   CMPLEN,L'CMPCODE+L'CMPLEN+L'CMPSEQ+L'CMPPROG                     
*                                                                               
         STC   R3,CMPSEQ                                                        
         MVC   CMPPROG,0(R5)                                                    
         OC    CMPPROG,SPACES                                                   
         GOTO1 ADDELEM                                                          
         DROP  RE                                                               
*                                                                               
WPJD70   LA    R3,1(R3)                                                         
         LA    R2,L'CMSSTA(R2)     NEXT STATION                                 
         LA    R5,L'SVPJPRGS(R5)   NEXT PROGRAM NAME                            
         B     WPJD50                                                           
*                                                                               
WPJD80   TM    LCHG,LCPPT+LCRAT+LCSHR   NEED TO UPDATE DEMO ELEMENTS            
         BZ    WPJD190                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CMDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   WPJD150             NO DEMO ELEMENT - SO ADD IT                  
*                                                                               
         USING CMDEL,R6                                                         
WPJD90   CLC   CMDTYP,SVDEM                                                     
         BE    WPJD100             FOUND                                        
         BAS   RE,NEXTEL                                                        
         BNE   WPJD150             THIS DEMO ELEM NOT FOUND - SO ADD IT         
         B     WPJD90                                                           
*                                  R6 - A(DEMO ELEM)                            
WPJD100  MVC   CMDPUT,SVPJPUT                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(CMDDEMO-CMDEL),CMDEL                                        
         LA    R3,CMDDEMO                                                       
         DROP  R6                                                               
*                                                                               
         L     RF,AIO                                                           
         USING CMPREC,RF                                                        
         LA    R2,CMSSTA                                                        
         LA    R4,CMSSEND                                                       
         DROP  RF                                                               
*                                                                               
         LA    R6,ELEM                                                          
         USING CMDEL,R6            BUILD THE DEMO VALUES ELEMENT                
         LA    RE,L'CMDCODE+L'CMDLEN+L'CMDTYP+L'CMDPUT                          
         STC   RE,CMDLEN           DEMO ELEM LEN W/O DEMO VALUES                
         LA    R5,CMDDEMO                                                       
         LA    RF,1                                                             
*                                                                               
WPJD105  CR    R2,R4               PASSED THE END OF STATION LIST               
         BNL   WPJD140                                                          
         CLI   0(R2),0             NO MORE STATION                              
         BE    WPJD140                                                          
*                                                                               
         ZIC   R0,SVSTANO                                                       
         LA    R1,SVSTA                                                         
         LA    RE,SVPROJ                                                        
*                                  SEARCH STATION ON THE SVSTA LIST             
WPJD110  CLC   0(L'CMSSTA,R2),0(R1)                                             
         BE    WPJD120             FOUND                                        
         LA    R1,L'SVSTA(R1)                                                   
         LA    RE,8(RE)            NEXT RATING+SHARE VALUE                      
         BCT   R0,WPJD110                                                       
*                                                                               
         MVC   0(L'CMDDEMO,R5),0(R3)                                            
         B     WPJD125                                                          
*                                                                               
WPJD120  STC   RF,0(R5)                                                         
         MVC   1(8,R5),0(RE)       RATING + SHARE                               
*                                                                               
WPJD125  LA    RF,1(RF)                                                         
         LA    R2,L'CMSSTA(R2)     NEXT STATION                                 
         LA    R3,L'CMDDEMO(R3)                                                 
         LA    R5,L'CMDDEMO(R5)                                                 
         ZIC   RE,CMDLEN                                                        
         AHI   RE,L'CMDDEMO                                                     
         STC   RE,CMDLEN                                                        
         B     WPJD105                                                          
*                                  REMOVE OLD AND ADD NEW DEMO ELEM             
WPJD140  GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),(4,SVDEM)                  
         GOTO1 ADDELEM                                                          
         B     WPJD190                                                          
         DROP  R6                                                               
*                                                                               
*                                                                               
WPJD150  L     RF,AIO                                                           
         USING CMPREC,RF                                                        
         LA    R2,CMSSTA                                                        
         LA    R4,CMSSEND                                                       
         DROP  RF                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CMDEL,R6            BUILD THE DEMO VALUES ELEMENT                
         MVI   CMDCODE,CMDCODEQ                                                 
*                                                                               
         ZIC   RE,SVSTANO          # STATIONS                                   
         MHI   RE,L'CMDDEMO                                                     
         AHI   RE,L'CMDCODE+L'CMDLEN+L'CMDTYP+L'CMDPUT                          
         STC   RE,CMDLEN           DEMO VALUES ELEMENT LENGTH                   
*                                                                               
         MVC   CMDTYP,SVDEM        DEMO CODE W/ FF                              
         MVC   CMDPUT,SVPJPUT      PJ PUT                                       
*                                                                               
         LA    R3,CMDDEMO                                                       
         LA    RF,1                STATION SEQ #                                
*                                                                               
WPJD160  CR    R2,R4               PASSED THE END OF STATION LIST               
         BNL   WPJD180                                                          
         CLI   0(R2),0             NO MORE STATION                              
         BE    WPJD180                                                          
*                                                                               
         ZIC   R0,SVSTANO                                                       
         LA    R1,SVSTA                                                         
         LA    R5,SVPROJ                                                        
*                                  SEARCH STATION ON THE SVSTA LIST             
WPJD165  CLC   0(L'CMSSTA,R2),0(R1)                                             
         BE    WPJD170             FOUND                                        
         LA    R1,L'SVSTA(R1)                                                   
         LA    R5,8(R5)            NEXT RATING+SHARE VALUE                      
         BCT   R0,WPJD165                                                       
         B     WPJD175                                                          
*                                                                               
WPJD170  STC   RF,0(R3)                                                         
         MVC   1(8,R3),0(R5)       RATING + SHARE                               
         LA    R3,L'CMDDEMO(R3)                                                 
*                                                                               
WPJD175  LA    RF,1(RF)                                                         
         LA    R5,8(R5)            NEXT RATING+SHARE VALUE                      
         LA    R2,L'CMSSTA(R2)     NEXT STATION                                 
         B     WPJD160                                                          
         DROP  R6                                                               
*                                                                               
WPJD180  GOTO1 ADDELEM                                                          
*                                                                               
WPJD190  GOTO1 PUTREC                                                           
         B     WPJDX                                                            
***************                                                                 
* ADDING THE COMPETITION RECORD                                                 
***************                                                                 
WPJD200  XC    KEY,KEY                                                          
         MVC   KEY(L'SVCMPKEY),SVCMPKEY                                         
*                                                                               
         L     RF,AIO              BUILD THE KEY                                
         MVC   0(L'SVCMPKEY,RF),SVCMPKEY                                        
         MVC   13(2,RF),=H'24'     RECORD SIZE                                  
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R6,ELEM                                                          
         USING CMSEL,R6            BUILD THE STATION LIST ELEMENT               
         MVI   CMSCODE,CMSCODEQ                                                 
         MVI   CMSLEN,21*L'CMSSTA+L'CMSLEN+L'CMSCODE                            
*                                                                               
         LA    R1,SVSTA                                                         
         LA    R2,CMSSTA                                                        
         ZIC   R3,SVSTANO                                                       
*                                                                               
         MVC   0(L'CMSSTA,R2),0(R1)                                             
         LA    R1,L'SVSTA(R1)                                                   
         LA    R2,L'CMSSTA(R2)                                                  
         BCT   R3,*-14                                                          
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING CMDEL,R6            BUILD THE DEMO VALUES ELEMENT                
         MVI   CMDCODE,CMDCODEQ                                                 
*                                                                               
         ZIC   RE,SVSTANO          # STATIONS                                   
         MHI   RE,L'CMDDEMO                                                     
         AHI   RE,L'CMDCODE+L'CMDLEN+L'CMDTYP+L'CMDPUT                          
         STC   RE,CMDLEN           DEMO VALUES ELEMENT LENGTH                   
*                                                                               
         MVC   CMDTYP,SVDEM        DEMO CODE W/ FF                              
         MVC   CMDPUT,SVPJPUT      PJ PUT                                       
*                                                                               
         LA    R1,SVPROJ                                                        
         LA    R2,CMDDEMO                                                       
         ZIC   R3,SVSTANO                                                       
         LA    RF,1                                                             
*                                                                               
WPJD210  STC   RF,0(R2)                                                         
         MVC   1(8,R2),0(R1)       RATING + SHARE                               
         LA    R1,8(R1)                                                         
         LA    R2,L'CMDDEMO(R2)                                                 
         LA    RF,1(RF)                                                         
         BCT   R3,WPJD210                                                       
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R1,SVPJPRGS         PROGRAM NAME LIST                            
         ZIC   R3,SVSTANO                                                       
         LA    R4,1                                                             
*                                                                               
WPJD220  XC    ELEM,ELEM                                                        
         USING CMPEL,R6            BUILD THE PROGRAM ELEMENT                    
         MVI   CMPCODE,CMPCODEQ                                                 
         MVI   CMPLEN,L'CMPCODE+L'CMPLEN+L'CMPSEQ+L'CMPPROG                     
*                                                                               
         STC   R4,CMPSEQ                                                        
         MVC   CMPPROG,0(R1)                                                    
         OC    CMPPROG,SPACES                                                   
         LA    R1,L'SVPJPRGS(R1)                                                
         LA    R4,1(R4)                                                         
         GOTO1 ADDELEM                                                          
         BCT   R3,WPJD220                                                       
*                                                                               
         GOTO1 ADDREC                                                           
         DROP  R6                                                               
*                                                                               
WPJDX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHANGES THE NWS RECORDS THAT HAVE THE SAME DAY/TIME/STA          
* AS THOSE LISTED IN ANWSTBL                                                    
***********************************************************************         
CHNGNWS  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         MVI   ACTELOPT,C'N'       DON'T PUT ACT ELEMENT TO NWS RECORD          
***********************************                                             
* GET THE STATION SEQUENCE NUMBER FROM THE COMP RECORD OF EACH STATION          
* IN NWSTBL SO WE CAN REFERENCE THE CHANGED DEMO RATING LATER                   
***********************************                                             
         L     R6,AIO2             AIO2 = A(COMP RECORD)                        
         USING CMPRECD,R6                                                       
         L     RE,ANWSTBL          WE SHOULD COPY STA SEQ FROM COMP REC         
         USING NWSTBLD,RE                                                       
CHNWS10  OC    0(NTBLLENQ,RE),0(RE)    DID ALL STA THAT WERE CHANGED?           
         BZ    CHNWS20                 YES                                      
         LA    R1,1                                                             
         LA    RF,CMSSTA                                                        
CHNWS12  CLC   NTBLQSTA,0(RF)                                                   
         BE    CHNWS14                                                          
         LA    RF,L'CMSSTA(RF)                                                  
         LA    R1,1(R1)                                                         
         CHI   R1,21                                                            
         BNH   CHNWS12                                                          
         B     XIT                 WE WILL BRANCH TO AN ERROR MESSAGE           
***      DC    H'0'                USING ACTELOPT = C'N'                        
                                                                                
*                                                                               
CHNWS14  STC   R1,NTBLBSTA                                                      
         LA    RE,NTBLLENQ(RE)                                                  
         L     R1,ANWSTBL          FOR ALL STA THAT WERE CHANGED                
         LA    R1,NMAXSTA*NTBLLENQ(R1)                                          
         CR    RE,R1                                                            
         BL    CHNWS10                                                          
         DROP  R6,RE                                                            
***********************************                                             
* GET THE NWS STATION SEQUENCE NUMBER SO WE CAN REFERENCE THE MINIO             
* RECORDS FASTER.  THIS IS IN THE NWS HEADER RECORD.                            
*                                                                               
* NOTE: THE STATION MIGHT NOT EXIST IN THE NWS HEADER BECAUSE THE USER          
*       COULD'VE CHANGED A STATION'S RTG/SHR/PUT THAT DOESN'T HAVE A            
*       WORK RECORD                                                             
***********************************                                             
CHNWS20  XC    KEY,KEY             GET THE HEADER RECORD                        
         LA    R6,KEY                                                           
         USING BWHKEY,R6                                                        
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM        THIS IS ALREADY ONES COMPLEMENTED            
         MVC   BWHKMKT,BMKT                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(BWHKSEQ-BWHKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BCMMKSEQ,BWHKSEQ    SAVE THE CAMP/MKT SEQ                        
         GOTO1 GETREC                                                           
***************                                                                 
* SCAN THRU ALL THE NWS HEADER'S STATION ELEMENTS                               
***************                                                                 
         L     R6,AIO1                                                          
         LA    R6,BWHFSTEL                                                      
         NI    BITFLAG1,X'FF'-BF1BUYRV                                          
         XR    R0,R0                                                            
CHNWS22  CLI   0(R6),0                                                          
         BE    CHNWS30                                                          
         CLI   0(R6),BWHELCDQ      STATION ELEMENT (X'02')?                     
         BE    CHNWS24                                                          
         CLI   0(R6),INFELCDQ      INFO ELEMENT (X'06')?                        
         BE    CHNWS28                                                          
CHNWS23  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHNWS22                                                          
*                                                                               
         USING BWHEL,R6                                                         
CHNWS24  L     RE,ANWSTBL          SEE IF THIS NWS STATION HAD ITS              
         USING NWSTBLD,RE            RATING CHANGED                             
         LA    RF,NMAXSTA*NTBLLENQ(RE)                                          
CHNWS25  DS    0H                                                               
         MVC   WORK(L'NTBLQSTA),NTBLQSTA                                        
         OI    WORK+4,C'T'         USE T INSTEAD OF BLANK                       
         CLC   BWHSTA,WORK                                                      
***      CLC   BWHSTA,NTBLQSTA                                                  
         BE    CHNWS26                                                          
         LA    RE,NTBLLENQ(RE)     CHECK AGAINST ALL STATIONS THAT              
         CR    RE,RF                 WERE CHANGED                               
         BL    CHNWS25                                                          
         B     CHNWS23                                                          
*                                                                               
CHNWS26  MVC   NTBLBNWS,BWHSEQ                                                  
         B     CHNWS23                                                          
*                                                                               
         USING INFELD,R6                                                        
CHNWS28  TM    INFFLAG1,IFF1BYRV   THIS CAMP/MKT DOING BUY REVISIONS?           
         BZ    *+8                                                              
         OI    BITFLAG1,BF1BUYRV   YES, DON'T HAVE TO CHANGE WORK RECS          
         B     CHNWS23                                                          
         DROP  R6,RE                                                            
***********************************                                             
* NOW WE NEED TO SORT ANWSTBL BY NWS STA SEQ ORDER SO WE CAN GO THRU            
* THE NWS MINIO RECORDS FASTER.  ALSO GETTING RID OF STATIONS THAT ARE          
* NOT IN THE NWS HEADER RECORD.                                                 
***********************************                                             
CHNWS30  TM    BITFLAG1,BF1BUYRV   CAMP/MKT DOING BUY REVISIONS?                
         BNZ   CHNWS100                                                         
         TM    SVBWDFLG,NKMFBUYR   NO, BUT DID WE COME FROM BUY/SKED?           
         BNZ   CHNWSX                  YES, THEN DON'T UPDATE WORK RECS         
*                                                                               
         L     RE,ANWSTBL          RE = A(WHERE TO STORE LOWEST ENTRY)          
         LA    RF,NMAXSTA*NTBLLENQ(RE)     RF = A(END OF TABLE)                 
CHNWS33  LR    R1,RE               R1 = A(CURRENT ENTRY) PARSING THRU           
         XR    R6,R6               R6 = A(LOWEST ENTRY SO FAR)                  
*                                                                               
         USING NWSTBLD,R1                                                       
CHNWS36  CLI   NTBLBNWS,0          STATION DOESN'T EXIST IN NWS HDR?            
         BNE   *+14                                                             
         XC    0(NTBLLENQ,R1),0(R1)                                             
         B     CHNWS37                                                          
*                                                                               
         LTR   R6,R6               DID WE FIND AN ENTRY BEFORE?                 
         BNZ   *+10                                                             
         LR    R6,R1               NO, LOWEST ENTRY IS CURRENT ENTRY            
         B     CHNWS37                                                          
*                                                                               
         CLC   NTBLBNWS,NTBLBNWS-NWSTBLD(R6)  CURRENT < LOWEST SO FAR?          
         BNL   CHNWS37                                                          
         LR    R6,R1                           YES, CURRENT NOW LOWEST          
*                                                                               
CHNWS37  LA    R1,NTBLLENQ(R1)     PARSE THRU ALL ENTRIES                       
         CR    R1,RF               DONE PARSING THIS NTH PASS?                  
         BL    CHNWS36             NO, STILL MORE                               
*                                                                               
         LTR   R6,R6                    ANY MORE TO SORT?                       
         BZ    CHNWS30X                 NO MORE                                 
         CR    RE,R6               SAME ADDRESS?                                
         BE    CHNWS38             YES, DON'T BOTHER                            
         XC    0(NTBLLENQ,RE),0(R6)     YES, SWAP ENTRIES                       
         XC    0(NTBLLENQ,R6),0(RE)                                             
         XC    0(NTBLLENQ,RE),0(R6)                                             
*                                                                               
CHNWS38  LA    RE,NTBLLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    CHNWS33                                                          
*                                                                               
CHNWS30X DS    0H                                                               
         DROP  R1                                                               
*                                                                               
CHNWS40  TM    BITFLAG1,BF1BUYRV   THIS CAMP/MKT DOES BUY REVISIONS?            
         BNZ   CHNWS100            YES                                          
***********************************                                             
* FOR ALL ENTRIES IN ANWSTBL, GOT THRU ALL NWS DETAIL RECORDS THAT HAVE         
* THE SAME DAYS/TIMES AND THEN MODIFY THE RATINGS TO MATCH THE RATING           
* FOR THE DEMO THAT WAS CHANGED.                                                
***********************************                                             
         L     R4,ANWSTBL                                                       
         USING NWSTBLD,R4                                                       
CHNWS43  CLI   NTBLBNWS,0          ANY MORE IN ANWSTBL?                         
         BE    CHNWSX              NO MORE                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BWDKEY,R6                                                        
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BCMMKSEQ                                                 
         MVI   BWDKELCD,BWDELCDQ   MINIO GROUPS STARTS WITH X'01'               
         MVC   BWDKELST,NTBLBNWS                                                
         MVC   BWDKELDY,XFRMBDAY                                                
*                                                                               
         GOTO1 HIGH                                                             
CHNWS46  CLC   KEY(BWDKEL-BWDKEY),KEYSAVE                                       
         BNE   CHNWSX              HAS TO MATCH BYTES TO & INCL SEQ             
***************                                                                 
* GOT A NWS DETAIL RECORD.  THERE MIGHT BE PKG/ORB DETAILS.                     
*                                                                               
* NOTE:  IF BWDKELST=NTBLBNWS THEN THERE CAN POTENTIALLY BE MORE FOR            
*         THE STATION (MINIO SPLIT ALGORITHM)                                   
***************                                                                 
         GOTO1 GETREC              IF BWDKELST=NTBLBNWS THEN THERE CAN          
         NI    BITFLAG1,X'FF'-BF1PUTRC  DON'T NEED TO PUTREC YET                
         L     R6,AIO                  POTENTIALLY BE MORE FOR THE STA          
         LA    R6,BWDFSTEL         FIRST ELEMENT                                
CHNWS50  CLI   0(R6),0             END OF THIS RECORD                           
         BE    CHNWS80             SEE IF WE NEED TO WRITE OUT THIS REC         
         CLI   0(R6),BWDELCDQ      NEED TO FIND X'01' ELEM                      
         BE    CHNWS60                                                          
CHNWS55  XR    R0,R0               NEXT DETAIL RECORD                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHNWS50                                                          
*                                                                               
         USING BWDEL,R6                                                         
CHNWS60  CLC   BWDSTACD,NTBLBNWS   NEED TO MATCH STATION WE'RE AT               
         BL    CHNWS55                                                          
         BH    CHNWS80             NO MORE FOR THE STATION WE'RE AT             
*                                                                               
         CLC   BWDDAYS,XFRMBDAY                                                 
         BNE   CHNWS55                                                          
         CLC   BWDTIMES,XFRMBTIM                                                
         BNE   CHNWS55                                                          
*                                                                               
         CLC   QSTA,NTBLQSTA       IF STATION MATCHES THE STA IN KEY            
         BNE   CHNWS61                                                          
         CLC   BWDSEQ,SVBWDSEQ     THEN IT SHOULD MATCH THE SEQUENCE            
         BNE   CHNWS55                                                          
*                                                                               
CHNWS61  TM    NTBLFLAG,NTBLFPGM   PROGRAM NAME CHANGE?                         
         BZ    CHNWS62                                                          
         LA    R3,BWDPROG                                                       
         BAS   RE,RPLPRGRM         YES, REPLACE THE PROGRAM                     
         OI    BITFLAG1,BF1PUTRC                                                
*                                                                               
CHNWS62  TM    NTBLFLAG,NTBLFRTG   RATING CHANGE?                               
         BZ    CHNWS80             NO, SEE IF WE NEED TO WRITE RECORD           
         XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),DMOELCDQ      NEED TO LOOK AT DEMO ELEM                    
         BNE   CHNWS80                                                          
         USING DMOEL,R6                                                         
         XR    R2,R2                                                            
         IC    R2,1(R6)                                                         
         AR    R2,R6               R2=A(BYTE AFTER END OF DEMO ELEM)            
         LA    R3,DMODEMO                                                       
CHNWS65  CLC   SVDEM(3),0(R3)      MATCH THE DEMO CATEGORY?                     
         BE    CHNWS70                                                          
         LA    R3,L'DMODEMO(R3)                                                 
         CR    R3,R2                                                            
         BL    CHNWS65                                                          
         B     CHNWS80             WE DON'T HAVE THIS DEMO                      
*                                                                               
CHNWS70  BAS   RE,RPLRATNG         REPLACE THE RATING W/ WHAT WE HAVE           
         BNE   CHNWS80                                                          
         OI    BITFLAG1,BF1PUTRC                                                
*                                                                               
CHNWS80  TM    BITFLAG1,BF1PUTRC   DO WE WRITE THIS RECORD BACK OUT?            
         BZ    CHNWS85                                                          
         GOTO1 PUTREC              YES, WRITE ONCE PER STATION                  
         B     CHNWS90                                                          
*                                                                               
CHNWS85  L     R6,AIO                                                           
         USING BWDKEY,R6                                                        
         CLC   BWDKELST,NTBLBNWS   STA SEQ IN THE KEY MATCH OURS?               
         BH    CHNWS90             NO, IT'S HIGHER, SO WE DONE WITH STA         
         GOTO1 SEQ                                                              
         B     CHNWS46             CHECK NEXT NWS DETAIL RECORD                 
         DROP  R6                                                               
***************                                                                 
* LOOP FOR ALL ENTRIES IN ANWSTBL                                               
***************                                                                 
CHNWS90  LA    R4,NTBLLENQ(R4)                                                  
         L     RF,ANWSTBL                                                       
         LA    RF,NMAXSTA*NTBLLENQ(RF)                                          
         CR    R4,RF                                                            
         BL    CHNWS43                                                          
         B     CHNWSX                                                           
         EJECT                                                                  
***********************************                                             
* FOR ALL ENTRIES IN ANWSTBL, GOT THRU ALL NWS REVISION RECORDS THAT            
* HAVE THE SAME DAYS/TIMES AND THEN MODIFY THE RATINGS TO MATCH THE             
* RATING FOR THE DEMO THAT WAS CHANGED.                                         
***********************************                                             
CHNWS100 TM    SVBWDFLG,NKMFBUYR   CAME FROM BUY/SKED?                          
         BZ    CHNWSX              NO, DON'T UPDATE BUY REVISION RECS           
*                                                                               
         L     R4,ANWSTBL                                                       
         USING NWSTBLD,R4                                                       
CHNWS110 OC    0(NTBLLENQ,R4),0(R4)   ANY MORE ENTRIES?                         
         BZ    CHNWSX                   NO MORE                                 
*                                                                               
         GOTO1 MSPACK,DMCB,QMKT,NTBLQSTA,WORK                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NBRKEY,R6                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMMKSEQ                                                 
         MVC   NBRKSTA,WORK+2                                                   
*                                                                               
         CLC   QSTA,NTBLQSTA                                                    
         BNE   *+16                                                             
         MVC   NBRKKBUY,SVBWDBBY                                                
         MVC   NBRKNBSQ,SVBWDSEQ                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                  KEY MATCHES UP TO AND INCL STATION?          
CHNWS120 CLC   KEY(NBRKKBUY-NBRKEY),KEYSAVE                                     
         BNE   CHNWS190            NO, NEXT STATION THEN                        
*                                                                               
         CLC   QSTA,NTBLQSTA                                                    
         BNE   CHNWS125                                                         
         CLC   NBRKKBUY(L'NBRKKBUY+L'NBRKNBSQ),KEYSAVE+NBRKKBUY-NBRKEY          
         BNE   CHNWS190                                                         
*                                                                               
CHNWS125 GOTO1 GETREC                                                           
         NI    BITFLAG1,X'FF'-BF1PUTRC   DON'T NEED TO PUTREC YET               
         L     R6,AIO                                                           
         LA    R6,NBRFSTEL                                                      
         USING NBRSELD,R6                                                       
         CLC   NBRSDAYS,XFRMBDAY                                                
         BNE   CHNWS180                                                         
         CLC   NBRSTIMS(4),XFRMBTIM                                             
         BNE   CHNWS180                                                         
*                                                                               
         TM    NTBLFLAG,NTBLFPGM   PROGRAM NAME CHANGE?                         
         BZ    CHNWS130                                                         
         LA    R3,NBRSPROG                                                      
         BAS   RE,RPLPRGRM                                                      
         OI    BITFLAG1,BF1PUTRC                                                
*                                                                               
CHNWS130 TM    NTBLFLAG,NTBLFRTG   RATING CHANGE?                               
         BZ    CHNWS150                                                         
CHNWS132 XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHNWS150                                                         
         CLI   0(R6),NBRDMELQ      DEMO ELEM (X'20')?                           
         BNE   CHNWS132                                                         
         USING NBRDMELD,R6                                                      
         XR    R2,R2                                                            
         IC    R2,1(R6)                                                         
         AR    R2,R6               R2=A(BYTE AFTER END OF DEMO ELEM)            
         LA    R3,NBRDMDMO                                                      
CHNWS134 CLC   SVDEM(3),0(R3)      MATCH THE DEMO CATEGORY?                     
         BE    CHNWS136                                                         
         LA    R3,L'DMODEMO(R3)                                                 
         CR    R3,R2                                                            
         BL    CHNWS134                                                         
         B     CHNWS150            WE DON'T HAVE THIS DEMO                      
*                                                                               
CHNWS136 BAS   RE,RPLRATNG         REPLACE THE RATING W/ WHAT WE HAVE           
         BNE   CHNWS150                                                         
         OI    BITFLAG1,BF1PUTRC                                                
*                                                                               
CHNWS150 TM    BITFLAG1,BF1PUTRC   DO WE WRITE THIS RECORD BACK OUT?            
         BZ    CHNWS190                                                         
         GOTO1 PUTREC                                                           
         B     CHNWS190                                                         
*                                                                               
CHNWS180 GOTO1 SEQ                                                              
         B     CHNWS120                                                         
***************                                                                 
* LOOP FOR ALL ENTRIES IN ANWSTBL                                               
***************                                                                 
CHNWS190 LA    R4,NTBLLENQ(R4)                                                  
         L     RF,ANWSTBL                                                       
         LA    RF,NMAXSTA*NTBLLENQ(RF)                                          
         CR    R4,RF                                                            
         BL    CHNWS110                                                         
*                                                                               
CHNWSX   MVI   ACTELOPT,C'Y'                                                    
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE REPLACE THE PROGRAM NAME IN THE DETAIL'S ELEM WITH WHAT          
* THE USER INPUT IN THE COMP RECORD                                             
*                                                                               
* ON ENTRY:    AIO2                A(NWS COMP RECORD)                           
*              (R4)                A(NWS TABLE ENTRY)                           
*              (R3)                A(PROGRAM NAME IN DESC ELEM)                 
***********************************************************************         
RPLPRGRM NTR1                                                                   
         USING NWSTBLD,R4                                                       
         L     R6,AIO2             FIND THE PROGRAM NAME FOR STATION            
         USING CMPRECD,R6                                                       
         LA    R6,CMSEL            POINT TO FIRST ELEM                          
RPLP10   CLI   0(R6),0             NO DEMO THAT MATCHES?                        
         BE    RPLPX               NO, THEN NOTHING TO REPLACE                  
         CLI   0(R6),CMPCODEQ      PROGRAM ELEM (X'03')?                        
         BE    RPLP20                                                           
RPLP15   XR    R0,R0               LOOP TIL WE REACH DEMO USER CHANGED          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RPLP10                                                           
*                                                                               
         USING CMPEL,R6                                                         
RPLP20   CLC   NTBLBSTA,CMPSEQ     HAS TO MATCH THE STATION                     
         BNE   RPLP15                                                           
*                                                                               
         MVC   0(L'BWDPROG,R3),SPACES    REPLACE THE PROGRAM                    
         MVC   0(L'CMPPROG,R3),CMPPROG                                          
*                                                                               
RPLPX    B     XIT                                                              
         DROP  R4,R6                                                            
***********************************************************************         
* THIS ROUTINE REPLACE THE RATING FOUND IN THE DETAIL'S DEMO ELEM WITH          
* WHAT THE USER INPUT IN THE COMP RECORD                                        
*                                                                               
* ON ENTRY:    AIO2                A(NWS COMP RECORD)                           
*              (R4)                A(NWS TABLE ENTRY)                           
*              (R3)                A(DEMO IN THE DEMO ELEM)                     
***********************************************************************         
RPLRATNG NTR1                                                                   
         USING NWSTBLD,R4                                                       
         L     R6,AIO2                                                          
         USING CMPRECD,R6                                                       
         LA    R6,CMSEL            POINT TO FIRST ELEM                          
RPLR10   CLI   0(R6),0             NO DEMO THAT MATCHES?                        
         BE    RPLRNO              NO, THEN NOTHING TO REPLACE                  
         CLI   0(R6),CMDCODEQ      DEMO ELEM?                                   
         BE    RPLR20                                                           
RPLR15   XR    R0,R0               LOOP TIL WE REACH DEMO USER CHANGED          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RPLR10                                                           
*                                                                               
         USING CMDEL,R6                                                         
RPLR20   CLC   SVDEM(3),CMDTYP     HAS TO MATCH DEMO USER CHANGED               
         BNE   RPLR15                                                           
         LA    R2,CMDDEMO          WE NEED TO FIND THE STA                      
RPLR25   CLC   NTBLBSTA,0(R2)                                                   
         BE    RPLR30                                                           
         LA    R2,L'CMDDEMO(R2)                                                 
         B     RPLR25                                                           
*                                                                               
RPLR30   MVC   4(4,R3),1(R2)       REPLACE THE RATING                           
*                                                                               
RPLRYES  SR    RC,RC                                                            
RPLRNO   LTR   RC,RC                                                            
         B     XIT                                                              
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       SETUP                                         *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A22'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DS    H'0'                                                             
         MVC   VSPDEMUP,DMCB                                                    
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A21'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DS    H'0'                                                             
         MVC   VSPDEMLK,DMCB                                                    
*                                                                               
         CLI   16(RA),0            FIRST TIME RUN                               
         BNE   SUP10                                                            
         MVI   16(RA),1            FLIP THE 1ST TIME FLAG                       
         MVI   TWASTA,X'FF'        INITIALIZE STATION POINTER                   
*                                                                               
         MVI   USEIO,C'Y'                                                       
*                                                                               
         LA    R0,NMAXSTA                                                       
         LA    R1,SVSTA                                                         
         XC    0(L'SVSTA,R1),0(R1)                                              
         LA    R1,L'SVSTA(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         MVI   STANDIS,0           STATION # ON DISPLAY ON 1ST LINE             
*                                                                               
         B     SETUPX                                                           
*                                                                               
SUP10    DS    0H                                                               
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
CMPRECD  DSECT                                                                  
       ++INCLUDE SPNWSCOMP                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
         ORG   SYSSPARE                                                         
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
*                                                                               
ASVDEMVL DS    A                                                                
ASVPROGS DS    A                                                                
*                                                                               
CMDDMEND DS    XL4                 END OF CMDDEMOS SAVED TO COMPARE             
*                                                                               
SVPROJ   DS    (NMAXSTA*2)XL4           CURRENT  PROJ DEMO R/S VALUES           
SVPROJL  EQU   *-SVPROJ                                                         
OLDPROJ  DS    (NMAXSTA*2)XL4           ORIGINAL PROJ DEMO R/S VALUES           
*                                                                               
SVPJPRGS DS    (NMAXSTA)XL(L'L1PJPROG+1)  CURRENT  PROJ PROG NAMES W/           
SVPJPRGL EQU   *-SVPJPRGS                    LAST BYTE FOR X'80'                
*                                                                               
SVDEMVAL DS    (NMAXSTA*2*4)XL4                                                 
SVDEMVLL EQU   *-SVDEMVAL                                                       
*                                                                               
SVPROGS  DS    (NMAXSTA*4)XL(L'L1PROG1)                                         
SVPROGSL EQU   *-SVPROGS                                                        
*                                                                               
NMAXSTA  EQU   21                  MAX STATIONS                                 
OVERELEM EQU   X'DE'               MAX STATIONS                                 
         EJECT                                                                  
***********************************************************************         
LINE1D   DSECT                                                                  
L1STA    DS    CL8                                                              
         DS    XL2                                                              
L1PROG1  DS    CL12                                                             
         DS    XL2                                                              
L1PROG2  DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
L1LENQ   EQU   *-LINE1D                                                         
         SPACE 2                                                                
LINE2D   DSECT                                                                  
         DS    CL10                                                             
L2RS1    DS    CL12                                                             
         DS    XL2                                                              
L2RS2    DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
         DS    XL2                                                              
         DS    CL12                                                             
L2LENQ   EQU   *-LINE2D                                                         
         DS    XL2                                                              
L1PJPROG DS    CL12                                                             
         SPACE 2                                                                
CUMLINED DSECT                                                                  
         DS    XL6                                                              
CUM1     DS    CL5                                                              
         DS    XL9                                                              
CUM2     DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
         DS    CL5                                                              
         DS    XL9                                                              
CUMPROJ  DS    CL5                                                              
***********************************                                             
* THE FOLLOWING DSECT IS COPIED FROM SPNWS14 (NEW COMP RECORD)                  
***********************************                                             
NUKEM1D  DSECT                     TEXT DATA BLOCK FOR SPOT/SFM - NUKEM         
NUKM1MED DS    CL1                 MEDIA                                        
NUKM1BYR DS    CL3                 BUYER                                        
NUKM1CAM DS    CL5                 CAMPAIGN                                     
NUKM1STA DS    CL8                 STATION                                      
NUKM1DYS DS    CL7                 DAYS                                         
NUKM1TMS DS    CL11                TIMES                                        
NUKM1DSL DS    CL5                 DAYPART/SLN                                  
NUKM1DMO DS    CL7                 DEMO                                         
NUKM1WKY DS    CL8                 WEEKLY                                       
***M1BKS DS    4CL7                4 BOOKS                                      
NUKM1BKS DS    4CL8                4 BOOKS                                      
NUKM1ODY DS    XL1                 OVERRIDE DAY                                 
NUKM1OTM DS    XL4                 OVERRIDE TIME                                
NUKM1UFL DS    CL1                 UPGRADE FILE                                 
NUKM1UEQ DS    XL8                 UPGRADE EQUATION                             
NUKM1UPG DS    CL32                UPGRADE                                      
NUKM1FBK DS    XL2                 UPGRADE FROM BOOK                            
NUKM1UPT DS    CL1                 UPGRADE PUT AVERAGING                        
NUKM1USH DS    CL1                 UPGRADE SHR AVERAGING                        
NUKM1BLS DS    XL6                 UPGRADE FROM BOOK LIST                       
NUKM1LNQ EQU   *-NUKEM1D           LENGTH OF THIS DATA BLOCK                    
NUKM1FBT DS    XL1                 NUKM1FBK BOOK TYPE                           
         DS    XL8                 SPARE                                        
NUKM1LQ2 EQU   *-NUKEM1D           EXTENDED LENGTH                              
*                                                                               
NUKEM2D  DSECT                     TEXT DATA BLOCK FOR SPOT/SFM - NUKEM         
NUKM2PRG DS    CL17                PROGRAM NAME                                 
NUKM2BBY DS    XL3                 BUYKBUY                                      
NUKM2SEQ DS    XL1                 SEQ NUMBER FOR DUPLICATES (BWDSEQ)           
NUKM2RTG DS    XL4                 RATING PASSED TO NEWCOMP                     
NUKM2FLG DS    XL1                 MISC FLAG BITS                               
NKMFBUYR EQU   X'80'                - CALLED FROM BUY RECORD                    
NUKM2LPM DS    XL2                 LPM START DATE                               
NUKM2ALF DS    CL3                 ALPHA MARKET FOR SPDEMUP                     
NUKM2LNQ EQU   *-NUKEM2D           LENGTH OF THIS DATA BLOCK                    
*                                                                               
NWSTBLD  DSECT                                                                  
NTBLBNWS DS    XL1                 NWS HDR REC STA SEQ                          
NTBLQSTA DS    CL(L'SVSTA)         EBCDIC STATION CALL LETTERS                  
NTBLBSTA DS    XL1                 NWS COMP REC STA SEQ                         
NTBLFLAG DS    XL1                                                              
NTBLFRTG EQU   X'80'               RATING CHANGED                               
NTBLFPGM EQU   X'40'               PROGRAM CHANGED                              
NTBLLENQ EQU   *-NWSTBLD                                                        
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
*******  PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM63D          MAINTENACE SCREEN                            
         ORG   COMWORK                                                          
**********   ORG   T217FFD+2700                                                 
********************************************************************            
** CAREFUL GENCON USES TWA STORAGE AT X'E00'                                    
********************************************************************            
*                                                                               
       ++INCLUDE DEDBLOCK          DBLOCK FOR DEMAND CALL                       
         EJECT                                                                  
       ++INCLUDE DDEBLOCK          EBLOCK FOR EDICTOR CALL                      
         EJECT                                                                  
XFRMDAY  DS    CL(L'COMDAY)                                                     
XFRMTIM  DS    CL(L'COMTIM)                                                     
XFRMDLN  DS    CL(L'COMDLN)                                                     
XFRMBDAY DS    XL(L'DAY)                                                        
XFRMBTIM DS    XL(L'TIME)                                                       
XFRMDPT  DS    CL(L'DPT)                                                        
XFRMSLN  DS    XL(L'SLN)                                                        
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
SVATWA   DS    A                   SAVED ATWA                                   
ANWSTBL  DS    A                   NWS STATION TABLE                            
ERRNUM   DS    XL2                 ERROR NUMBER                                 
TWASTA   DS    X                   TWA STATION POINTER                          
SVCMPKEY DS    CL13                SAVED COMPETE KEY                            
BBYRMASK DS    XL1                 MASK TO A/M FOR BUYER PAST 1ST 255           
BBYR     DS    X                   BUYER CODE  (ONES COMPLEMENT)                
BCAM     DS    XL2                 CAMPAIGN    (ONES COMPLEMENT)                
BCMMKSEQ DS    XL2                 CAMPAIGN/MARKET SEQUENCE NUMBER              
DAY      DS    X                   DAYS OF THE WEEK                             
ESTOWSDY DS    X                   ESTIMATE OUT OF WEEK START DAY               
ESTDMENU DS    X                   ESTIMATE DAYPART MENU NUMBER                 
ESTBKTYP DS    CL1                 OVERRIDING BOOKTYPE                          
TIME     DS    0XL4                TIME                                         
STIME    DS    XL2                 STARTING TIME                                
ETIME    DS    XL2                 ENDING TIME                                  
CMPSLN   DS    X                   CAMPAIGN SPOT LENGTH                         
DPT      DS    X                   DAYPART                                      
QDPT     DS    CL3                 DAYPART TIME SHEET PRINT                     
DPTSUBS  DS    XL16                SUB-DAYPARTS (16X1)                          
DPTTYPE  DS    CL1                 DAYPART TYPE                                 
*                                  C'R' = REGULAR                               
*                                  C'M' = MASTER                                
*                                  C'S' = SUB                                   
CMPDPOPT DS    CL1                 CAMPAIGN DAYPART OPTION                      
*                                  C'M' = SCHEDULE SUBDPTS UNDER MASTER         
*                                  C'S' = SCHEDULE SUBDPTS SEPERATELY           
*                                                                               
CMPRSRC  DS    X                   CAMPAIGN RATING SOURCE OVERRIDE              
CMPOPTS  DS    X                   CAMPAIGN OPTIONS                             
CMPRSVC  DS    C                   CAMPAIGN RATING SOURCE                       
CMPSCHEM DS    CL3                 CAMPAIGN SID SCHEME                          
CMPPSCHM DS    XL2                 CAMPAIGN SID SCHEME PACKED                   
CLTOFF   DS    X                   CLIENT OFFICE NUMBER                         
CLTSRC   DS    X                   CLIENT RATING SERVICE CODE                   
CLTBWPRO DS    CL16                CLIENT BW PROFILE                            
SLN      DS    X                   SPOT LENGTH                                  
*SVDEM    DS    XL3                 DEMO CODE                                   
*         DS    X                   SPACE FOR END OF DEMO LIST MARKER           
ESTDMLST DS    20XL3               ESTIMATE DEMO CODE LIST                      
*                                                                               
SVBKS    DS    XL8                 4 BOOKS DATES (2 BYTE EACH)                  
SVBKTPS  DS    XL4                 SVBK BOOKTYPES (BINARY)                      
SVWKY    DS    XL2                 WEEKLY: W1,W2,W3,W4                          
SVWKYIND DS    XL1                 WEEKLY INDICATOR                             
SVONEBK  EQU   X'80'                                                            
*                                                                               
LCHG     DS    X                                                                
LCDEM    EQU   X'80'                                                            
LCBKS    EQU   X'40'                                                            
LCPPT    EQU   X'20'                                                            
LCPGS    EQU   X'10'                                                            
LCRAT    EQU   X'08'                                                            
LCSHR    EQU   X'04'                                                            
LCNST    EQU   X'02'               NEW STATION ADDED                            
*                                                                               
G1WPROF  DS    XL16                                                             
STANDIS  DS    X                   STATION # DISPLAYED ON THE 1ST LINE          
*                                                                               
CMPUF    DS    CL1                 CAMPAIGN UPGRADE FILE                        
CMPUP    DS    XL8                 CAMPAIGN UPGRADE EXPRESSION                  
CMPUPIN  DS    CL32                CAMPAIGN UPGRADE AS INPUT                    
CMPFB    DS    XL2                 CAMPAIGN OVERRIDE FROM BOOK                  
CMPFBTP  DS    XL1                 CMPFB BOOK TYPE (BINARY)                     
CMPUPUT  DS    CL1                 CAMPAIGN UPGRADE PUT AVERAGING               
CMPUSHR  DS    CL1                 CAMPAIGN UPGRADE SHR AVERAGING               
CMPFBLST DS    XL6                 CAMPAIGN FROM BOOK LIST (MAX 3)              
*                                                                               
BITFLAG1 DS    XL1                 MISCELLANEOUS FLAGS                          
BF1XFRCN EQU   X'80'                - WE USED GLOBBER TO GET HERE               
BF1PUTRC EQU   X'40'                - WE NEED TO PUTREC                         
BF1BUYRV EQU   X'40'                - WE NEED TO PUTREC                         
BF1TWODC EQU   X'02'                - NEED 2 DECIMALS                           
BF1KYCHG EQU   X'01'                - KEY FIELDS HAVE CHANGED                   
XFRFRSYS DS    CL3                 GLVXCTL  FROM  SYS                           
XFRFRPRG DS    CL3                 GLVXCTL  FROM  PRG                           
*                                                                               
CLTIND2  DS    XL1                 MORE CLIENT INDICATORS                       
CLTIANFR EQU   X'80'               1W PROFILE CANADIAN ANGLO/FRANCO IND         
CLTIREQC EQU   X'40'               ROUND EQUIVALENCED COSTS                     
CLTISDLY EQU   X'20'               SEPARATE LINES FOR DAILY SCHED               
CLTIEST  EQU   X'10'               CAMPAIGN DATES MUST MATCH ESTIMATE           
CLTIXOUT EQU   X'08'               X-OUT SCHEDULED WEEKS FOR COPY               
CLTIUS   EQU   X'04'               CANADIAN DEMO OPTION = U                     
CLTPONLY EQU   X'02'               POL ONLY                                     
CLTFROZN EQU   X'01'               CLIENT FROZEN                                
*                                                                               
NOSTA    DS    X                   # STATIONS ON DISPLAY                        
NWSHBSTA DS    X                   STATION SEQ NUMBER IN NWH                    
STABKTYP DS    CL1                 STATION SPECIAL BOOK TYPE                    
*                                                                               
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
*                                                                               
ASVSTA   DS    A                                                                
SVSTA    DS    (NMAXSTA)XL10                                                    
SVSTANO  DS    X                                                                
*                                                                               
VSPDEMUP DS    F                                                                
VSPDEMLK DS    F                                                                
SVDEMLK  DS    XL32                                                             
*                                                                               
SVPJCUM  DS    F                                                                
SVPJPUT  DS    F                                                                
SVCUMS   DS    XL(4*4)                                                          
SVPUTVAL DS    XL(4*4)                                                          
*                                                                               
SVFLAG   DS    XL1                                                              
SVF2STA  EQU   X'80'                                                            
SVDEM    DS    XL4                                                              
SVPUT    DS    XL4                                                              
SVRTGSHR DS    XL7                                                              
*                                                                               
SVUFILE  DS    C                                                                
SVUGRD   DS    XL8                                                              
SVUFRBK  DS    XL2                                                              
SVUFRBT  DS    XL1                 SVUFRBK BOOK TYPE (BINARY)                   
SVUFBL   DS    XL6                                                              
SVUPUT   DS    CL1                                                              
SVUSHR   DS    CL1                                                              
SVUINP   DS    CL32                                                             
SVUDAY   DS    X                                                                
SVUTIM   DS    XL4                                                              
SVRTGSVC DS    C                                                                
*====== SEE NUKEM2D === START BLOCK ====                                        
SVBWDPRG DS    CL17                                                             
SVBWDBBY DS    XL3                 BUY DETAILS                                  
SVBWDSEQ DS    X                                                                
SVBWDRTG DS    XL4                                                              
SVBWDFLG DS    XL1                                                              
SVMLPMSD DS    XL2                 LPM START DATE                               
SVMALPHA DS    CL3                 ALPHA MARKET                                 
*====== SEE NUKEM2D === END BLOCK ======                                        
LDMUPBLK DS    (SPDEMUP2)X                                                      
*                                                                               
*                                                                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE SPNWSBYR          BYRRECD                                      
       ++INCLUDE SPNWSCAM          CAMRECD                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST          ESTIMATE HEADER RECORD DSECT                 
DPTHDRD  DSECT                                                                  
       ++INCLUDE SPGENDAYPT        DAYPART HEADER RECORD DSECT                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT HEADER RECORD DSECT                   
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA          STATION RECORD DSECT                         
* SPDEMUPD                                                                      
       ++INCLUDE SPDEMUPD          SPDEMUPD                                     
* SPGENCLST                                                                     
       ++INCLUDE SPGENCLST                                                      
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPNWSDTL                                                       
       ++INCLUDE SPNWSBRV                                                       
* SPDEMLKXTD                                                                    
       ++INCLUDE SPDEMLKXTD                                                     
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DEDEMTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPSFM57   05/13/09'                                      
         END                                                                    
