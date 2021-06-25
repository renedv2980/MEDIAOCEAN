*          DATA SET SPSFM62    AT LEVEL 195 AS OF 10/20/06                      
*PHASE T21762A                                                                  
*INCLUDE XSORT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21762  --  DAYPART           MAINTENANCE & LIST     *         
*                                                                     *         
*  COMMENTS:     MAINTAINS DAYPART RECORDS                            *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM5A (MAINT) & SCSFM59 (LIST)              *         
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
         TITLE 'T21762 - DAYPART MAINTENANCE AND LIST'                          
T21762   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1762**,R7,RR=R3                                              
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
         CLI   MODE,VALKEY          VALIDATE RECORD KEY                         
         BE    VK                                                               
         CLI   MODE,VALREC          VALIDATE RECORD                             
         BE    VR                                                               
         CLI   MODE,DISPKEY         DISPLAY KEY (FOR LIST)                      
         BE    DK                                                               
         CLI   MODE,DISPREC         DISPLAY RECORD                              
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                   LIST RECORDS                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
                                                                                
         XC    FLAG,FLAG                                                        
         CLI   ACTEQU,ACTLIST       IS IT A LIST ACTION?                        
         BNE   VK10                 NO...VALIDATE MNT SCREEN'S KEY              
         XC    TEMPMENU,TEMPMENU    HOLDS THE MENU FOR BOTH SCREENS             
         LA    R2,LSTMEDH           R2 PTR TO MEDIA FLD HEADER ON LIST          
         GOTO1 VALIMED              VALIDATE THE MEDIA                          
         MVC   LSTMEDE,MEDNM        MOVE THE MEDIA NAME ON THE SCREEN           
         OI    LSTMEDEH+6,X'80'     TRANSMIT                                    
         CLI   LSTMENUH+5,0         MENU FILTER GIVEN ON LIST SCREEN?           
         BE    *+10                 NO                                          
         MVC   TEMPMENU,LSTMENU     MOVE THE MENU IN                            
         B     VK20                                                             
                                                                                
VK10     LA    R2,MNTMEDSH          R2 POINTER TO MEDIA FIELD HEADER            
         GOTO1 VALIMED              VALIDATE MEDIA                              
         MVC   MNTMEDE,MEDNM        DISPLAY MEDIA ON SCREEN                     
         OI    MNTMEDEH+6,X'80'     TRANSMIT                                    
         MVC   TEMPMENU,MNTMNUS     STORE MENU                                  
         LA    R2,MNTMNUSH          R2 POINTER TO MENU FIELD HEADER             
         GOTO1 ANY                                                              
         TM    4(R2),X'08'          IS IT NUMERIC?                              
         BO    VK15                 YES                                         
         TM    4(R2),X'04'          IS IT ALPHABETIC?                           
         BNO   ERRALPH              NOT ALPHANUMERIC...ERROR                    
VK15     BAS   RE,VALSORT           VALIDATE THE SORT FILTER                    
VK20     LA    R4,KEY                                                           
         USING DAYPTD,R4                                                        
         XC    KEY,KEY              CLEAR THE KEY                               
         MVI   DPTKTYPE,DAYPTQ      RECORD TYPE (X'08')                         
         MVC   DPTKAGY,AGENCY       AGENCY CODE                                 
         MVC   DPTKMED,QMED         MEDIA CODE                                  
         MVC   DPTKMENU,TEMPMENU    MENU                                        
         DROP  R4                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         BAS   RE,VALSORT           VALIDATE THE SORT FILTER                    
         XC    WORK2(6),WORK2       SAVE 'Z' ENTRY HERE/WORK2+5 = COUNT         
         MVC   ZEXTN,=C'   '        SAVE 'Z' EXTENSION HERE                     
         XC    FLAG,FLAG                                                        
         LA    R2,MNTSTRTH          LETTER/MS CODE/TIME SHEET HEADER            
         GOTO1 ANY                                                              
*                                                                               
         BAS   RE,CHECKZ                                                        
*                                                                               
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR50                 YES                                         
         BAS   RE,CKINSDEL          CHECK FOR INSERT AND DELETES                
         CLI   FLAG,0                                                           
         BNE   DR                                                               
*                                                                               
         LA    R2,MNTSTRTH          LETTER/MS CODE/TIME SHEET HEADER            
         MVI   ELCODE,X'01'         LETTER/MS CODE/TIME SHEET ELEMENT           
         GOTO1 REMELEM              REMOVE THE ELEMENT                          
         MVI   ELCODE,X'02'         EXTENSION ELEMENT                           
         GOTO1 REMELEM              REMOVE THE ELEMENT                          
*                                                                               
VR50     LA    R5,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         USING DPTEL,R5             R5 IS USED FOR FIRST ELEMENT                
         XC    ELEM,ELEM                                                        
         MVI   DPTEL,X'01'          MOVE THE ELEMENT CODE IN                    
         MVI   DPTELEN,DPTEL2-DPTEL ELEMENT LENGTH (188)                        
         LA    R5,2(R5)             MOVE POINTER TO FIRST BYTE OF DATA          
*                                                                               
         LA    R6,ELEM2             256 BYTE FIELD TO HOLD TEMP ELEMENT         
         USING DPTEL2,R6            R6 USED AS SECOND ELEMENT REGISTER          
         XC    ELEM2,ELEM2                                                      
         MVI   DPTEL2,X'02'         MOVE THE ELEMENT CODE IN                    
         MVI   DPTELEN2,DPTEL2-DPTEL ELEMENT LENGTH (188)                       
         LA    R6,2(R6)             MOVE POINTER TO FIRST BYTE OF DATA          
*                                                                               
VR100    CLI   5(R2),0              IS THERE ANY INPUT DATA?                    
         BNE   VR105                YES                                         
         TM    FLAG,FOUNDZZZ        DID ZZZ COME UP YET?                        
         BO    VR200                YES                                         
         B     ERRBLK               BLANK BEFORE ZZZ - INVALID                  
VR105    CLI   5(R2),7              LENGTH FOR FIELDS WITH NO EXTENSION         
         BE    VR110                                                            
         CLI   15(R2),C'='          EXTENSION FIELDS MUST HAVE C'='             
         BNE   ERREXT1                                                          
         CLI   5(R2),9              AND AT LEAST ONE CHAR FOR THE EXTN          
         BL    ERREXT2                                                          
VR110    CLI   8(R2),C'$'           SPECIAL CODES FOR COKE                      
         BE    VR140                AND ANYONE ELSE STUPID ENOUGH               
         CLI   8(R2),C'+'           TO TYPE ONE OF THEM IN                      
         BE    VR140                                                            
         CLI   8(R2),C'-'                                                       
         BE    VR140                                                            
         CLI   8(R2),C'@'           ALLOW @ FOR SEGRAGATING WEB BUYS            
         BE    VR140                                                            
         CLI   8(R2),X'F0'          BETWEEN 1 AND 9 ?                           
         BL    VR120                                                            
         CLI   8(R2),X'F9'                                                      
         BH    ERRALPH                                                          
         B     VR140                                                            
VR120    CLI   8(R2),X'C1'          IS THE FIELD VALID ALPHABETIC?              
         BL    ERRALPH                                                          
         CLI   8(R2),X'E9'                                                      
         BH    ERRALPH                                                          
VR140    BAS   RE,CHKDUP            CHECK FOR DUPLICATE ENTRIES                 
*                                                                               
         MVC   0(1,R5),8(R2)        LETTER                                      
         LA    R7,9(R2)             VALIDATE AND TRANSLATE TO HEX               
         BAS   RE,HEXTRAN           SUBROUTINE RETURNS ANS IN HEXBYTE           
         ZIC   R7,HEXBYTE                                                       
         SLL   R7,4                 SHIFT TO HI ORDER NIBBLE                    
         LR    R3,R7                SAVE OFF HIGH ORDER NIBBLE                  
         LA    R7,10(R2)            VALIDATE AND TRANSLATE TO HEX               
         BAS   RE,HEXTRAN           SUBROUTINE RETURNS ANS IN HEXBYTE           
         ZIC   R7,HEXBYTE                                                       
         AR    R7,R3                BRING TOGETHER                              
         STC   R7,1(R5)             2 BYTE MS CODE STORED AS 1 BYTE             
         MVC   2(3,R5),12(R2)       TIME SHEET HEADER                           
         CLI   0(R5),C'Z'           IS IT A Z?                                  
         BNE   VR160                NO                                          
         OI    FLAG,FOUNDZZZ        ZZZ CAME UP IN TIME SHEET HEADER            
         OC    WORK2(5),WORK2       IS WORK2 EMPTY?                             
         BNZ   ERRINV               NO...ERROR                                  
         MVC   WORK2(5),0(R5)                                                   
         XC    0(5,R5),0(R5)                                                    
         CLI   5(R2),7              IF 'Z' EXTENSION IS GIVEN                   
         BNH   VR200                                                            
         MVC   ZEXTN,16(R2)         MOVE IT TO ZEXTN                            
         OC    ZEXTN,=C'   '        BLANK OUT ANY NULLS                         
         OI    FLAG,FOUNDLM2        SET THE FOUND ELEMENT 2 FLAG                
         GOTO1 CHKEXTN,DMCB,ZEXTN   VALIDATE EXTENSION                          
         B     VR200                                                            
*                                                                               
VR160    CLI   5(R2),7              IF EXTENSION IS GIVEN                       
         BNH   VR170                                                            
         MVC   0(3,R6),16(R2)       MOVE IT TO EXTENSION ELEMENT                
         OC    0(3,R6),=C'   '      BLANK OUT ANY NULLS                         
         OI    FLAG,FOUNDLM2        SET THE FOUND ELEMENT 2 FLAG                
         GOTO1 CHKEXTN,DMCB,(R6)    VALIDATE EXTENSION                          
*                                                                               
VR170    OC    0(3,R6),=C'   '      BLANK OUT ANY NULLS                         
         LA    R5,5(R5)             BUMP TO NEXT DATA IN ELEMENT 1              
         LA    R6,5(R6)             BUMP TO NEXT DATA IN ELEMENT 2              
         ZIC   RE,WORK2+5           CURRENT COUNT OF ENTRIES                    
         LA    RE,1(RE)             ADD 1 TO CURRENT COUNT OF ENTRIES           
         STC   RE,WORK2+5           NEW CURRENT COUNT OF ENTRIES                
VR200    ZIC   RE,0(R2)             BUMP TO NEXT FIELD ON SCREEN                
         AR    R2,RE                                                            
         CLI   0(R2),9              END OF SCREEN?                              
         BNE   VR100                NO                                          
         SPACE                                                                  
         OC    WORK2(5),WORK2       ANY ENTRIES?                                
         BZ    ERRMIS               NO...ERROR                                  
         SPACE 2                                                                
         CLI   MNTSORT,C'Y'         SORT ON MS CODES?                           
         BNE   VR300                NO                                          
*                                                                               
         ZIC   R3,WORK2+5           TOTAL NUMBER OF DAYPT ENTRIES               
         CHI   R3,1                 AT LEAST 2?                                 
         BNH   VR300                NO                                          
         BAS   RE,SORTDPT           SORT ON MS CODES                            
                                                                                
VR300    MVC   0(5,R5),WORK2        MOVE LAST DAYPT IN ELEMENT 1                
         MVC   0(3,R6),ZEXTN        MOVE LAST DAYPT EXT IN ELEMENT 2            
         DROP  R5                                                               
         DROP  R6                                                               
*                                                                               
         L     R4,AIO               BUILD RECORD                                
         USING DAYPTD,R4                                                        
         MVC   DPTEL(188),ELEM      BUILD FIRST ELEMENT (REQUIRED)              
         CLI   MNTSORT,C'Y'         DID USER WANT SORT?                         
         BNE   VR305                NO                                          
         OI    DPTFLAG,X'80'        TURN ON SORT FLAG IN RECORD                 
         B     VR310                                                            
VR305    NI    DPTFLAG,X'7F'        TURN SORT FLAG OFF IN RECORD                
VR310    TM    FLAG,FOUNDLM2        DID WE FIND A SECOND ELEMENT?               
         BZ    VRX                  NO                                          
         MVC   DPTEL2(188),ELEM2    BUILD SECOND ELEMENT                        
         MVC   DPTLEN,=H'400'                                                   
         DROP  R4                                                               
*                                                                               
VRX      B     DR                   REDISPLAY RECORD                            
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
*                                                                               
         LA    R2,MNTSTRTH          FIRST DISPLAY                               
         SR    R3,R3                                                            
*                                                                               
DR105    ZIC   RF,0(R2)             HEADER + DATA                               
         SHI   RF,9                 SUBTRACT HEADER LENGTH +1                   
         TM    1(R2),X'02'          IS THERE AN EXTENDED FIELD HEADER?          
         BZ    *+8                  NO                                          
         SHI   RF,8                 SUBTRACT EXTENDED HEADER LENGTH             
*                                                                               
         EX    RF,*+8               PAD WITH BLANKS                             
         B     *+10                                                             
         MVC   8(0,R2),=11X'40'                                                 
         OI    6(R2),X'80'          TRANSMIT                                    
*                                                                               
         ZIC   R0,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R0                                                            
         CLI   0(R2),9              IS END OF SCREEN?                           
         BNE   DR105                NO                                          
*                                                                               
         LA    R2,MNTSTRTH          FIRST FIELD HEADER                          
         L     R6,AIO                                                           
         MVI   ELCODE,1             ELEMENT X'01'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BE    *+6                  THIS ELEMENT IS REQUIRED                    
         DC    H'0'                                                             
         LA    R5,2(R6)             R5 PTR TO FIRST ELEMENT DATA                
*                                                                               
DR119    L     R6,AIO                                                           
         MVI   ELCODE,2             ELEMENT X'02'                               
         BAS   RE,GETEL             GET EXT ELEMENT                             
         BNE   DR120                ELEMENT NOT REQUIRED                        
         OI    FLAG,LM2EXIST        TURN ON ELEM2 FOUND FLAG                    
         LA    R6,2(R6)             BUMP PAST ELEMENT CODE AND LENGTH           
                                                                                
DR120    TM    FLAG,INSERTON        DID USER TYPE INSERT?                       
         BNO   DR121                NO                                          
         C     R2,INSADDR           DID USER WANT TO INSERT HERE?               
         BNE   DR121                NO                                          
         LR    R3,R2                YES..SAVE OFF CURSOR POSITION               
         B     DR126                YES...LEAVE IT BLANK                        
                                                                                
DR121    MVC   8(1,R2),0(R5)        MOVE THE RECORD TO THE SCREEN               
         OI    6(R2),X'80'          TRANSMIT                                    
         ZIC   R7,1(R5)             "PACKED NUM" 28 MUST CONVERT F2 F8          
         SRL   R7,4                 WANT TO DISPLAY NUM ON LEFT FIRST           
         STC   R7,HEXBYTE           MOVE 02 TO HEXBYTE                          
         LA    R7,HEXBYTE           R7 POINTER TO HEXBYTE                       
         BAS   RE,HEXTRAN2          TRANSLATE 2 TO F2                           
         MVC   9(1,R2),HEXBYTE      MOVE F2 TO SCREEN                           
                                                                                
         MVC   HEXBYTE,1(R5)        "PACKED NUM" 28 MUST CONVERT F2 F8          
         NI    HEXBYTE,X'0F'        WANT TO DISPLAY NUM ON RIGHT FIRST          
         LA    R7,HEXBYTE           R7 POINTER TO HEXBYTE                       
         BAS   RE,HEXTRAN2          TRANSLATE 8 TO F8                           
         MVC   10(1,R2),HEXBYTE     MOVE F8 TO SCREEN                           
*                                                                               
         MVI   11(R2),C'='          MOVE = SIGN TO SCREEN                       
         MVC   12(3,R2),2(R5)       DISPLAY TIME SHEET PRINT                    
         AHI   R5,5                 BUMP TO NEXT DATA IN ELEMENT 1              
         OI    4(R2),X'20'          FIELD HAS BEEN PREVIOUSLY VALIDATED         
         TM    FLAG,LM2EXIST        DOES ELEMENT 2 EXIST?                       
         BNO   DR126                NO                                          
         CLI   0(R6),X'40'          IS THE EXTENSION ELEMENT BLANK?             
         BNH   DR124                YES                                         
         MVI   15(R2),C'='          MOVE = SIGN IN                              
         MVC   16(3,R2),0(R6)       MOVE THE EXTENSION IN                       
DR124    AHI   R6,5                 BUMP TO NEXT DATA IN ELEMENT 2              
DR126    ZIC   R1,0(R2)             HEADER+DATA LENGTH                          
         AR    R2,R1                BUMP TO NEXT ENTRY ON SCREEN                
         CLI   0(R5),0              END OF ELEMENT DATA?                        
         BNE   DR120                NO                                          
*                                                                               
         CHI   R3,0                 DID WE SAVE OFF "INS" CURSOR ADDRS?         
         BE    DRX                  NO                                          
         LR    R2,R3                YES...MOVE CURSOR THERE                     
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R4,AIO                                                           
         USING DAYPTD,R4                                                        
         MVC   MNTMEDS,3(R4)        DISPLAY MEDIA LETTER ON SCREEN              
         OI    MNTMEDSH+6,X'80'     TRANSMIT                                    
         MVC   MNTMEDE,MEDNM        DISPLAY MEDIA ON SCREEN                     
         OI    MNTMEDEH+6,X'80'     TRANSMIT                                    
         MVC   MNTMNUS,4(R4)        DISPLAY MENU ON SCREEN                      
         OI    MNTMNUSH+6,X'80'     TRANSMIT                                    
         TM    DPTFLAG,X'80'        IS SORT FLAG ON?                            
         BO    DK10                 YES                                         
         MVI   MNTSORT,C'N'         NO SORT..BUT N IN SORT FILTER FIELD         
         B     DK20                                                             
DK10     MVI   MNTSORT,C'Y'         BUT Y IN SORT FILTER FIELD                  
DK20     OI    MNTSORTH+6,X'80'                                                 
         DROP  R4                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
                                                                                
         LA    R4,KEY               R4 POINTER TO KEY                           
         OC    KEY,KEY              FIRST TIME THROUGH?                         
         BNZ   LR10                 NO                                          
                                                                                
         USING DAYPTD,R4                                                        
         MVI   DPTKTYPE,DAYPTQ      RECORD TYPE (X'08')                         
         MVC   DPTKAGY,AGENCY       AGENCY CODE                                 
         MVC   DPTKMED,QMED         MEDIA CODE                                  
         MVC   DPTKMENU,TEMPMENU    MENU                                        
         DROP  R4                                                               
         MVC   SAVEKEY,KEY          SAVE THE KEY                                
LR10     GOTO1 HIGH                 FIRST RECORD                                
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON HIGH            
                                                                                
LR20     LA    R4,KEY               R4 POINTER TO KEY                           
         GOTO1 SEQ                  NEXT RECORD                                 
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON SEQ             
                                                                                
LR30     CLC   KEY(4),SAVEKEY       SAME SYSTEM/PROGRAM?                        
         BNE   LRX                  NO MORE DATATYPES TO LIST                   
                                                                                
         MVC   LSMENU,KEY+4         MOVE MENU TO SCREEN                         
                                                                                
         GOTO1 GETREC                                                           
         GOTO1 LISTMON              SEND RECORD TO SCREEN                       
         B     LR20                 NEXT RECORD                                 
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
                                                                                
CHECKZ   NTR1                                                                   
*                                                                               
         LA    R2,MNTSTRTH                                                      
CKZ10    CLI   8(R2),C'Z'                                                       
         BE    CKZX                                                             
         CLI   5(R2),0                                                          
         BE    CKZ20                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),9              END OF SCREEN?                              
         BNE   CKZ10                                                            
CKZ20    B     ERRNOZ                                                           
CKZX     XIT1                                                                   
***********************************************************************         
*                          CKINSDEL                                   *         
***********************************************************************         
CKINSDEL NTR1                                                                   
                                                                                
         SR    R3,R3                R3 = NUMBER OF DAYPT BEFORE DELETE          
         SR    R1,R1                R1 POINTER TO INSERT                        
         SR    R0,R0                R0 = TOTAL CHANGES                          
         XC    ZHOLD,ZHOLD                                                      
                                                                                
CL10     CLI   5(R2),0              IS THERE ANY INPUT DATA?                    
         BE    CL20                 NO                                          
         CLI   ZHOLD,C'Z'                                                       
         BE    CL11                                                             
         MVC   ZHOLD,8(R2)                                                      
CL11     TM    4(R2),X'20'          HAS DAYPT CHANGED SINCE DISPLAY?            
         BO    CL20                 NO                                          
         AHI   R0,1                                                             
         CLC   =C'INS',8(R2)        DOES USER WANT TO INSERT DATA?              
         BNE   CL15                 NO                                          
         TM    FLAG,INSERTON        AT LEAST ONE INSERT ALREADY?                
         BO    ERRINS               YES...ERROR                                 
         OI    FLAG,INSERTON        TURN ON INSERT FLAG                         
         AR    R1,R2                                                            
         SHI   R0,1                                                             
         B     CL20                                                             
CL15     CLC   =C'DEL',8(R2)        USER WANT TO DELETE DAYPT ENTRY?            
         BNE   CL19                 NO                                          
         OI    FLAG,DELETEON        TURN ON DELETE FLAG                         
         MVI   ELCODE,1                                                         
         BAS   RE,DELENTRY          DELETE ENTRY IN FIRST ELEMENT               
         MVI   ELCODE,2                                                         
         BAS   RE,DELENTRY          DELETE ENTRY IN SECOND ELEMENT              
         SHI   R0,1                                                             
         TM    FLAG,INSERTON        DID INSERT COME BEFORE DELETE?              
         BO    CL21                 YES                                         
         SHI   R1,19                                                            
         B     CL21                 DISPLAY THE REVISED RECORD                  
CL19     LR    R6,R2                SAVE OFF NON-INS/DEL CHANGE                 
         LA    R5,19(R2)            FIRST BLANK ENTRY                           
CL20     AHI   R3,1                 INCRIMENT INDEX INTO RECORD                 
CL21     ZIC   R4,0(R2)             BUMP                                        
         AR    R2,R4                                                            
         CLI   0(R2),9              LAST FIELD ON SCREEN?                       
         BNE   CL10                 NO                                          
*                                                                               
         CLI   ZHOLD,C'Z'                                                       
         BE    CL22                                                             
         LR    R2,R5                                                            
         B     ERRNOZ                                                           
CL22     CHI   R0,0                 TRIES TO INS/DEL AND CHANGE DATA?           
         BE    CL25                 NO                                          
         TM    FLAG,INSERTON        INSERT ON + DATA CHANGE?                    
         BO    CL24                 YES...ERROR                                 
         TM    FLAG,DELETEON        DELETE ON + DATA CHANGE?                    
         BNO   CLX                  NO...EXIT AND VALIDATE DATA                 
CL24     LR    R2,R6                                                            
         B     ERRCHG               YES                                         
CL25     TM    FLAG,INSERTON        DID USER WANT TO INSERT?                    
         BNO   CL50                 NO                                          
         TM    FLAG,DELETEON        DID USER WANT A DELETE?                     
         BO    CL50                 YES                                         
         SHI   R2,19                LAST DAYPART ON SCREEN                      
         LR    R3,R2                SAVE LOCATION OF LAST FIELD                 
         LR    R2,R1                POINT R2 TO "INS" IN CASE OF ERROR          
         CLI   5(R3),0              IS IT BLANK?                                
         BNE   ERRINS2              NO...ERROR                                  
CL50     ST    R1,INSADDR           SAVES THE LOCATION OF INSERT                
CLX      B     XIT                                                              
***********************************************************************         
*                          HEXTRAN                                    *         
***********************************************************************         
HEXTRAN  NTR1                                                                   
         LA    R3,HEXTAB            R3 POINTER TO HEX TRANSLATE TABLE           
TR100    CLI   1(R3),0              DID WE REACH END OF TABLE?                  
         BE    ERRTAB               YES...ERROR                                 
         CLC   0(1,R7),1(R3)        DID WE FIND A MATCH?                        
         BE    TR200                YES                                         
         LA    R3,2(R3)             NO MATCH...BUMP TABLE                       
         B     TR100                                                            
TR200    MVC   HEXBYTE,0(R3)        STORE THE HEX EQUIVALENT                    
         XIT1                                                                   
                                                                                
***********************************************************************         
*                          CHKDUP                                     *         
***********************************************************************         
CHKDUP   NTR1                                                                   
         USING DPTEL,R5                                                         
         LA    R5,ELEM                                                          
         LA    R1,DPTCODES                                                      
CD100    CLI   0(R1),0              END OF LIST                                 
         BE    XIT                  YES - THEN ADD THIS                         
         CLC   0(1,R1),8(R2)        DID WE FIND A DUPLICATE MS CODE?            
         BE    ERRDUP               YES...ERROR                                 
         LA    R1,5(R1)             NEXT ENTRY                                  
         B     CD100                                                            
         DROP  R5                                                               
***********************************************************************         
*                          HEXTRAN2                                   *         
***********************************************************************         
HEXTRAN2 NTR1                                                                   
         LA    R3,HEXTAB            R3 POINTER TO HEX TRANSLATE TABLE           
HT100    CLI   1(R3),0              DID WE REACH END OF TABLE?                  
         BE    ERRTAB               YES...ERROR                                 
         CLC   0(1,R7),0(R3)        DID WE FIND A MATCH?                        
         BE    HT200                YES                                         
         LA    R3,2(R3)             NO MATCH...BUMP TABLE                       
         B     HT100                                                            
HT200    MVC   HEXBYTE,1(R3)        STORE THE HEX EQUIVALENT                    
         XIT1                                                                   
                                                                                
***********************************************************************         
*                          SORTDPT                                    *         
***********************************************************************         
SORTDPT  NTR1                                                                   
                                                                                
         LA    R2,ELEM+2            POINT TO DATA IN ELEMENT 1                  
         LA    R5,ELEM2+2           POINT TO DATA IN ELEMENT 2                  
         LA    R4,ELEM3             ELEM 3 IS BUFFER TO SORT MERGED             
                                                                                
SD10     CLI   0(R2),0              END OF ELEMENT DATA?                        
         BE    SD20                 YES...SORT IT                               
         MVC   0(5,R4),0(R2)        MOVE DATA FROM ELEMENT 1 TO BUFFER          
         MVC   5(3,R4),0(R5)        MOVE DATA FROM ELEMENT 2 TO BUFFER          
         LA    R2,5(R2)             BUMP ELEMENT 1                              
         LA    R5,5(R5)             BUMP ELEMENT 2                              
         LA    R4,8(R4)             BUMP BUFFER                                 
         B     SD10                                                             
*                                                                               
SD20     GOTO1 =V(XSORT),DMCB,ELEM3,(R3),8,1,1,RR=RELO                          
*                                                                               
         LA    R2,ELEM+2            POINT TO DATA IN ELEMENT 1                  
         LA    R3,ELEM2+2           POINT TO DATA IN ELEMENT 2                  
         LA    R4,ELEM3             ELEM 3 IS BUFFER TO SORT MERGED             
*                                                                               
SD30     CLI   0(R2),0              END OF ELEMENT DATA?                        
         BE    SDX                  YES...RETURN                                
         MVC   0(5,R2),0(R4)        MOVE DATA FROM BUFFER TO ELEMENT 1          
         MVC   0(3,R3),5(R4)        MOVE DATA FROM BUFFER TO ELEMENT 2          
         LA    R2,5(R2)             BUMP ELEMENT 1                              
         LA    R3,5(R3)             BUMP ELEMENT 2                              
         LA    R4,8(R4)             BUMP BUFFER                                 
         B     SD30                                                             
*                                                                               
SDX      B     XIT                                                              
                                                                                
***********************************************************************         
*                          DELENTRY                                   *         
***********************************************************************         
DELENTRY NTR1                                                                   
                                                                                
         L     R6,AIO                                                           
         BAS   RE,GETEL             GET NAME ELEMENT                            
         BE    DEL08                SHOULD ALWAYS GET THE FIRST ELEMENT         
                                                                                
         CLI   ELCODE,1                                                         
         BNE   DELX                                                             
         DC    H'0'                 THIS ELEMENT IS REQUIRED                    
                                                                                
DEL08    LA    R6,2(R6)             BUMP PAST ELEMENT CODE AND LENGTH           
         MHI   R3,5                 ENTRY NUMBER TO DELETE                      
         AR    R6,R3                POINT TO DATA TO DELETE                     
         CLI   0(R6),C'Z'                                                       
         BNE   DEL10                                                            
         B     ERRDPT                                                           
DEL10    CLI   5(R6),0              IS NEXT ENTRY BLANK?                        
         BE    DEL20                YES                                         
         MVC   0(5,R6),5(R6)        CLOBBER CURRENT DATA W/ NEXT DATA           
         LA    R6,5(R6)             BUMP TO NEXT DATA                           
         B     DEL10                                                            
                                                                                
DEL20    XC    0(5,R6),0(R6)        CLEAR LAST ENTRY IN ELEMENT                 
DELX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          VALSORT                                    *         
***********************************************************************         
VALSORT  NTR1                                                                   
         LA    R2,MNTSORTH                                                      
         CLI   5(R2),0              ANY INPUT IN SORT FIELD?                    
         BE    VS01                 NO                                          
         CLI   8(R2),C'Y'           DID USER TYPE Y?                            
         BNE   VS02                 NO                                          
VS01     MVI   8(R2),C'Y'           MOVE A Y ON SCREEN (DEFAULT)                
         OI    6(R2),X'80'          TRANSMIT                                    
         B     VSX                                                              
VS02     CLI   8(R2),C'N'           DID USER TYPE N?                            
         BNE   ERRSORT              NO...USER IS STUPID                         
VSX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           SETUP                                     *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'     MODIFY SERVICE REQUEST                      
         OI    CONSERVH+6,X'80'     TRANSMIT TO GET CONTROL                     
         OI    GENSTAT4,NODELLST    CANNOT DELETE FROM LIST                     
*                                                                               
*** NON-DDS TERMINALS CAN ONLY DISPLAY,LIST, AND SELECT (READ ONLY)             
*                                                                               
         CLI   T217FFD+1,C'*'       TEST DDS TERMINAL                           
         BE    SETUPX                                                           
         CLI   ACTNUM,ACTCHA        ACTION CHANGE?                              
         BE    ERRSJR                                                           
         CLI   ACTNUM,ACTADD        ACTION ADD?                                 
         BE    ERRSJR                                                           
         CLI   ACTNUM,ACTDEL        ACTION DELETE?                              
         BE    ERRSJR                                                           
         CLI   ACTNUM,ACTREST       ACTION RESTORE?                             
         BE    ERRSJR                                                           
         CLI   THISLSEL,C'C'        SELECT FOR CHANGE?                          
         BE    ERRSJR                                                           
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          CHKEXTN                                    *         
***********************************************************************         
CHKEXTN  L     RF,0(R1)                                                         
         LA    R1,CHKEXTAB                                                      
*                                                                               
CE10     CLI   0(R1),X'FF'                                                      
         BE    ERREXT3                                                          
         CLC   0(3,RF),0(R1)                                                    
         BER   RE                                                               
         LA    R1,3(R1)                                                         
         B     CE10                                                             
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVC   ERRNUM,=AL2(21)      INVALID INPUT                               
         B     SPERREX                                                          
ERRMIS   MVC   ERRNUM,=AL2(1)       MISSING INPUT                               
         B     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(245)     DUPLICATE ENTRY                             
         B     SPERREX                                                          
ERRCHG   MVC   ERRNUM,=AL2(847)     CHANGE DATA WHILE DEL/INS                   
         B     SPERREX                                                          
ERRSJR   MVC   ERRNUM,=AL2(11)      THIS PROGRAM FOR DDS TERMINALS ONLY         
         B     SPERREX                                                          
ERRBLK   MVC   ERRNUM,=AL2(294)     BLANK ENTRY                                 
         B     SPERREX                                                          
ERRDPT   MVC   ERRNUM,=AL2(848)     DELETING LAST DAYPART                       
         B     SPERREX                                                          
ERRNOZ   MVC   ERRNUM,=AL2(849)     MISSING 'Z' ENTRY                           
         B     SPERREX                                                          
ERRALPH  MVC   ERRNUM,=AL2(916)     MUST BE ALPHA-NUMERIC                       
         B     SPERREX                                                          
ERREXT1  MVC   ERRNUM,=AL2(917)     EXTENTIONS MUST BEGIN WITH =                
         B     SPERREX                                                          
ERREXT2  MVC   ERRNUM,=AL2(918)     EXTENTION MUST BE AT LEAST 1 CHAR           
         B     SPERREX                                                          
ERREXT3  MVC   ERRNUM,=AL2(919)     PR,EM,DA,EF,EN,PA,LN,SP,CH,LF               
         B     SPERREX                                                          
ERRINS   MVC   ERRNUM,=AL2(920)     1 INSERT ALLOWED                            
         B     SPERREX                                                          
ERRINS2  MVC   ERRNUM,=AL2(921)     NO ROOM TO INSERT                           
         B     SPERREX                                                          
ERRTAB   MVC   ERRNUM,=AL2(922)     0-9, A-F                                    
         B     SPERREX                                                          
ERRSORT  MVC   ERRNUM,=AL2(923)     SORT OPTIONS ARE Y/N                        
         B     SPERREX                                                          
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
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
HEXTAB   DC    X'00F001F102F203F304F405F506F607F708F809F90AC10BC20CC30DX        
               C40EC50FC60000'                                                  
CHKEXTAB DC    C'PR ',C'EM ',C'DA ',C'EF ',C'EN ',C'PA ',C'LN ',C'SP '          
         DC    C'CH ',C'LF ',C'AC ',X'FF'                                       
                                                                                
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*SPSFMFFD                                                                       
*SCSFM5AD          MAINTENACE SCREEN                                            
*SCSFM59D          LIST SCREEN                                                  
*DDGENTWA                                                                       
*SPGENDAYPT        DAYPART RECORDS                                              
*FAGETTXTD         ERROR MSGS                                                   
*SPSFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM5AD          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM59D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
DAYPTD   DSECT                                                                  
       ++INCLUDE SPGENDAYPT        DAYPART RECORDS                              
DAYPTQ   EQU   X'08'                                                            
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
RELO     DS    A                   RELOCATION FACTOR                            
INSADDR  DS    A                   ADDRESS OF INSERT                            
*                                                                               
DPTCOUNT DS    H                   STORE NUMBER OF DAYPARTS                     
ERRNUM   DS    XL2                                                              
HEXBYTE  DS    X                                                                
ZEXTN    DS    CL3                 EXTENSION FOR Z DAYPART                      
FLAG     DS    C                   X'80' = ELEM INS='40' =                      
INSERTON EQU   X'01'               INSERT ENTRY                                 
DELETEON EQU   X'02'               DELETE ENTRY                                 
FOUNDZZZ EQU   X'04'               ZZZ ENTRY FOUND                              
FOUNDLM2 EQU   X'08'               EXTENTION ELEMENT FOUND                      
LM2EXIST EQU   X'10'               EXTENTION ELEMENT EXISTS                     
ELEM2    DS    CL188               AREA FOR EXTENTION ELEMENT                   
WORK2    DS    CL17                                                             
TEMPMENU DS    C                                                                
SAVEKEY  DS    CL10                                                             
ELEM3    DS    CL376               TEMP AREA FOR SORTING (KEEP EXT)             
ZHOLD    DS    C                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
         DS    CL2                                                              
LSMENU   DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'195SPSFM62   10/20/06'                                      
         END                                                                    
