*          DATA SET SPTRA0E    AT LEVEL 041 AS OF 05/12/14                      
*PHASE T2160EA                                                                  
         TITLE 'T2160E TEXT RECORD DISPLAY, CHANGE, ADD, DELETE, LIST'          
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 - REC READ IN  FOR CHANGE COMPARE                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - UNUSED                                                            
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
*  LEV 21    JAN06/87 CHANGE PRODUCT POL INVALID AND ADD PROFILE BY             
*                     OFFICE                                                    
*  LEV 22    MAR25/88 DON'T ALLOW FOOTNOTE TEXT FOR NETWORK                     
*  LEV 23    JAN26/89 FIX B FOR DISALLOW FOOTNOTES FOR NETWORK                  
*  LEV 24    FEB24/89 FIX VPRD TO VALIPRD                                       
*  LEV 25-28 DEC08/89 FIX BOXES FOR FOOTNOTES                         *         
*  LEV 29    JUN17/92 ADD AGENCY CODE TO REC                          *         
*  LEV 30    MAY06/93 ADD STRAFFIC SYSTEM                             *         
*  LEV 31    JUL20/94 CHANGE TO FILENAME                              *         
*  LEV 32    DEC13/94 SUPPRESS TYPE IF SPOTNETF IS N                  *         
*  LEV 33    FEB05/98 ADD PFKEY SUPPORT (IT'S ON THE SCREEN)          *         
*  LEV 34 SMUR OCT08/99 ALLOW MEDIA SPECIFIC TEXT RECORDS             *         
*  LEV 35 BGRI MAR27/01 PUT IN TRAFFIC OFFICE & FCLT SEC CK           *         
*  LEV 36 SMUR JUL02/01 FIX SECURITY LOCKOUT                          *         
*  LEV 37 SMUR JUN26/02 CLIENT STRING SECURITY                        *         
*  LEV 38 SMUR OCT09/03 BRAND LEVEL SECURITY                          *         
*  LEV 39 SMUR JUL26/03 SOX                                           *         
*  LEV 40 MHER MAY04/14 ADD MEDIA N REC WHEN ADDING T FOR CANADA      *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T2160E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TEXT**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         NI    GENSTAT2,X'FF'-USMYOK   RESET USE MY MSG FLAG                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    PUT                                                              
         CLI   MODE,XPREASON       EXPLAIN REASON CODE                          
         BE    EXPR                                                             
         CLI   MODE,XRECADD                                                     
         BE    AAR                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       BRAS  RE,VKEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                  FIRST VALIDATE PF KEY HIT                    
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    VRPFK5                                                           
         CLI   PFKEY,4             ADD LINE?                                    
         BE    VRPFK5                                                           
         CLI   PFKEY,0             ANY OTHER PFKEY IS INVALID                   
         BNE   INVPFKY                                                          
         B     VR00                ENTER KEY HIT, GO VALIDATE INPUT             
*                                                                               
VRPFK5   L     R5,ATIOB            A(TIOB)                                      
         USING TIOBD,R5                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R5                                                               
*                                                                               
         LA    R2,TRAL01H          1ST FIELD WHICH COULD CONTAIN CURSOR         
VRPFK10  SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               IS CURSOR ON 1ST TEXT LINE?                  
         BH    INVPOSN             NO -CURSOR IS ABOVE 1ST TEXT LINE            
         CR    RE,R1               END OF LINE ADDR, CURSOR ADDR                
         BNL   VRPFK20             BR IF NCURSOR IS BEFORE LINE END             
*                                                                               
         LA    RF,TRAL02H-TRAL01H  BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,TRAL09H                                                       
         CR    R2,RF               END OF SCREEN?                               
         BH    INVPOSN             YES- INVALID POSITION FOR PF3/4              
         B     VRPFK10                                                          
*                                                                               
VRPFK20  LA    RF,TRAL09H          A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VRPFK50             NO                                           
*                                                                               
         LA    R0,TRAL01H                                                       
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BL    VR00                YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURFORC         KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VRPFK30  CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VRPFK40             YES                                          
         LA    R0,TRAL02H-TRAL01H  LENGTH OF FIELD                              
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R2),1(R3)       MOVE ATTRIBUTE BYTE                          
         XC    4(2,R3),4(R3)       CLEAR LINE BELOW                             
         NI    1(R3),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         OI    6(R2),X'80'         TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST TEXT FIELD                        
         LR    R2,R3                                                            
         B     VRPFK30                                                          
*                                                                               
VRPFK40  XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R2),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR00                                                             
*                                                                               
VRPFK50  CLI   PFKEY,4             ADD LINE?                                    
         BNE   VRPFK80             NO                                           
*                                                                               
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR00                YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,TRAL09H          LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VRPFK60  LA    R0,TRAL02H-TRAL01H  DISTANCE TO NEXT LINE                        
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VRPFK70             YES                                          
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         OI    6(R3),X'80'         TRANSMIT                                     
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         MVC   1(1,R3),1(R2)       MOVE ATTRIBUTE BYTE                          
         LR    R3,R2                                                            
         B     VRPFK60                                                          
*                                                                               
VRPFK70  XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         NI    1(R3),X'FF'-X'0C'   FORCE NORMAL INTENSITY                       
         LA    R1,L'TRAL01                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURFORC         KEEP CURSOR IN PLACE                         
         B     VR00                                                             
*                                                                               
VRPFK80  TM    1(R2),X'08'         IS LINE CURRENTLY HIGH INTENSITY?            
         BZ    *+12                                                             
         NI    1(R2),X'FF'-X'0C'   YES -- FORCE NORMAL INTENSITY                
         B     *+8                                                              
         OI    1(R2),X'08'         NO -- FORCE HIGH INTENSITY                   
         EJECT                                                                  
*                                                                               
* ----HERE BEGINS THE NON-FUNCTION KEY VALIDATION FOR RECORD----                
*                                                                               
VR00     L     R4,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING TXTKEY,R4                                                        
         MVC   BAGYMD,TXTKAM                                                    
         CLC   BCLT,TXTKCLT        IS CLIENT SAME                               
         BE    VR02                YES                                          
*                                                                               
         BRAS  RE,FCLT             GO GET CLIENT SVCLIST                        
         BE    *+6                 SHOULD BE OKAY                               
         DC    H'0'                                                             
*                                                                               
VR02     MVC   QPRD,TXTKPROD                                                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VR02K                                                            
*                                                                               
         OC    QPRD,QPRD           ANY PROD                                     
         BNZ   VR02F                YES                                         
*                                                                               
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR02K                                                            
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
VR02F    LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
VR02H    CLC   QPRD,0(R1)                                                       
         BE    VR02K                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,VR02H                                                         
*                                                                               
         LA    R2,TRAPRDH          PRODUCT                                      
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
*                                                                               
VR02K    MVC   TYPE,TXTKTYP                                                     
         DROP  R4                                                               
         MVI   ELCODE,X'10'        BOXES ELEMENT                                
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE X'10' ELEMENT                    
         LA    R6,ELEM                                                          
         USING TXTBOXEL,R6                                                      
         MVI   TXTBOXEL,X'10'                                                   
         MVI   TXTBOXLN,3          ELEMENT LENGTH                               
         MVI   TXTBOX,0                                                         
         LA    R2,TRABOXH          FIRST TEXT LINE                              
         MVI   WORK,C'N'           DEFAULT TO NO                                
         CLI   TYPE,C'F'           IF FOOTNOTE, NO BOXES                        
         BNE   VR04                                                             
         CLI   5(R2),0             NO INPUT DEFAULT TO 'N'                      
         BE    VR03                                                             
         CLI   TRABOX,C'Y'         NO BOXES FOR FOOTNOTES                       
         BE    INVBOXER                                                         
         CLI   TRABOX,C'N'         NO BOXES FOR FOOTNOTES                       
         BNE   INVBOXER                                                         
VR03     MVI   TRABOX,C'N'                                                      
         OI    TRABOXH+6,X'80'     FORCE TRANSMIT                               
         MVI   WORK,C'N'                                                        
         MVC   MXTXTLNS,=H'3'      FOR FOOTNOTES                                
         B     VR06                                                             
VR04     GOTO1 ANY                                                              
         MVC   MXTXTLNS,=H'9'      IF NO BOXES                                  
         CLI   WORK,C'N'                                                        
         BE    VR06                                                             
         CLI   WORK,C'Y'                                                        
         BNE   INVBOXER                                                         
         MVC   MXTXTLNS,=H'7'      7 LINES MAX WITH BOXES                       
         OI    TXTBOX,X'80'                                                     
VR06     MVC   BOXES,WORK          SAVE BOX                                     
         GOTO1 ADDELEM                                                          
*                                                                               
* GET ANY TEXT LINES                                                            
*                                                                               
         MVI   ELCODE,X'40'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R3,1                SET TEXT LINE NUMBER                         
         LR    R5,R3               SET BLK LINE CTR                             
         LA    R6,ELEM                                                          
         USING TXTTXTEL,R6                                                      
         MVI   TXTTXTEL,X'40'                                                   
         LA    R2,TRAL01H          FIRST TEXT LINE                              
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
VR08     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR09                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR08             CK ENTIRE FLD                                
VR09     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
VR10     GOTO1 ANY                                                              
         CLI   TYPE,C'F'           FOOTNOTE TEXT                                
         BNE   VR12                NO                                           
         CH    R3,=H'3'            MAX OF 3 LINES OF FOOTNOTES                  
         BH    FTNTER                                                           
*                                                                               
VR12     CLI   BOXES,C'Y'          DOING BOXES                                  
         BE    VR14                YES                                          
         CLI   5(R2),58            MAX LENGTH                                   
         BH    TXTLENER            TOO LONG                                     
         B     VR16                                                             
VR14     CLI   5(R2),56            MAX LENGTH                                   
         BH    TXTLENER            TOO LONG                                     
VR16     CH    R3,MXTXTLNS         CK IF EXCEEDED MAX LINES                     
         BH    HDNTER                                                           
VR18     ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)           TOTAL ELEMENT LENGTH                         
         STC   R0,TXTTXTLN         AND LENGTH                                   
         STC   R3,TXTLNNUM         TEXT LINE NUMBER                             
         BCTR  R1,0                                                             
         EX    R1,VRMVC                                                         
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VR20     LA    R5,1(,R5)           INCREMENT BLANK LINE CTR                     
         ZIC   R1,0(R2)            GET THIS SCREEN FIELD LENGTH                 
         AR    R2,R1               ADD TO FIELD POINTER                         
         ZIC   R1,0(R2)            ALSO BYPASS PROTECTED FIELD                  
         AR    R2,R1               ADD TO FIELD POINTER                         
         LA    R1,TRATAGH          END OF SCREEN                                
         CR    R1,R2               IF THERE                                     
         BNH   VR30                GET OUT                                      
         LA    R0,L'TRAL01         MAX LENGTH OF FLD                            
         LA    R1,8+L'TRAL01-1(,R2)  ELIMINATE BLANKS FROM RT                   
VR22     CLI   0(R1),C' '          IF BLANK                                     
         BH    VR24                NON-BLANK                                    
         BCTR  R1,0                CK NEXT                                      
         BCT   R0,VR22             CK ENTIRE FLD                                
VR24     STC   R0,5(,R2)           STORE REAL LEN OF FLD                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR20                NO                                           
VR26     CR    R3,R5               CK LAST LINE VS THIS LINE                    
         BE    VR10                NO INTERVENING BLANK LINES                   
         MVI   TXTTXTLN,4          ONLY 1 BLANK NEEDED FOR BLK LINE             
         STC   R3,TXTLNNUM         STORE TEXT LINE NUMBER                       
         MVI   TXTTXT,C' '         AND HERE IT IS                               
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           ADD TO TEXT LINE CT                          
         B     VR26                BUILD ALL BLK LINES NEEDED                   
VR30     CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    VR34                YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BE    VR34                NO                                           
         MVC   CHREASON,=C'NC'                                                  
         B     *+10                                                             
VR34     MVC   CHREASON,=C'TC'     SET UP AS MAINT CHANGE                       
         CLI   ACTNUM,ACTADD       UNLESS ADD                                   
         BNE   VR36                                                             
         MVI   CHREASON+1,C'A'                                                  
VR36     B     DR                  NOW DISPLAY VALIDATED RECORD                 
VRMVC    MVC   TXTTXT,WORK                                                      
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         MVI   TRABOX,C'N'                                                      
         BAS   RE,GETEL                                                         
         BNE   DR04                                                             
         USING TXTBOXEL,R6                                                      
         MVI   TRABOX,C'N'                                                      
         TM    TXTBOX,X'80'                                                     
         BZ    DR04                                                             
         MVI   TRABOX,C'Y'                                                      
DR04     OI    TRABOXH+6,X'80'                                                  
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                HAS TO BE 1 TEXT ELEMENT                     
         LA    R2,TRAL01H                                                       
         LA    R4,8(,R2)                                                        
         USING TXTTXTEL,R6                                                      
DR10     ZIC   R1,TXTTXTLN         GET TEXT ELEMENT LENGTH                      
         SH    R1,=H'4'            GET TEXT LENGTH -1                           
         ZIC   RF,0(R2)            GET FIELD LENGTH                             
         LR    RE,RF                                                            
         SH    RF,=H'9'            GET FIELD LENGTH -1                          
         EX    RF,DRXC             CLEAR OUTPUT FLD                             
         CR    RF,R1               SEE IF ENOUGH ROOM IN FIELD                  
         BNL   DR20                                                             
         DC    H'0'                NOT ENOUGH ROOM IN FLD FOR TEXT LINE         
DR20     EX    R1,DRMVC                                                         
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
         AR    R2,RE               POINT TO NEXT FIELD (TITLE)                  
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD (INPUT)                  
         BAS   RE,NEXTEL                                                        
         BNE   DR30                                                             
         LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BH    DR10                NO                                           
         DC    H'0'                MORE TEXT LINES THAN SCREEN SPACE            
DR30     LA    R0,TRATAGH                                                       
         CR    R0,R2               CK IF END OF SCREEN                          
         BNH   DR40                NO                                           
         OC    8(L'TRAL01,R2),8(R2)                                             
         BZ    DR32                                                             
         XC    8(L'TRAL01,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
DR32     ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT TITLE                                 
         ZIC   R1,0(R2)            GET FLD LEN                                  
         AR    R2,R1               NOW AT NEXT INPUT FIELD                      
         B     DR30                                                             
*                                                                               
* FORCE MODIFIED TO STOP INVALID OR NO DATA REC ERR MSG FOR PFKEY               
*                                                                               
DR40     OI    TRATAGH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATAGH+6,X'80'     FORCE XMT OF TAB FIELD                       
         B     EXIT                                                             
*                                                                               
DRMVC    MVC   8(0,R2),TXTTXT                                                   
DRXC     XC    8(0,R2),8(R2)                                                    
         DROP  R6                                                               
         EJECT                                                                  
* GENERATE TURN-AROUND REQ AFTER ADDED REC                                      
* NOTE - SINCE NO MODE CHECK FOR XRECADD BEFORE I PUT IN THE CODE TO            
* ADD MEDIA N REC WHEN ADDING MEDIA T FOR CANADA, NOP GENR CALL                 
                                                                                
AAR      CLI   SPOTCAN,C'C'        TEST CANADA                                  
         JNE   AARX                                                             
         CLI   SVT3PROF+6,C'Y'     TEST AUTO ADD MEDIA N                        
         JNE   AARX                                                             
         CLI   TRAMED,C'T'         TEST SEL TV                                  
         JE    AAR2                                                             
         CLI   TRAMED,C'N'         TEST DOING N DIRECTLY                        
         JNE   AARX                                                             
         MVC   CONHEAD(40),=C'Media N REC added. Media T already there'         
         LA    R2,TRAMEDH                                                       
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
         J     EXIT                                                             
*                                                                               
AAR2     L     RE,AIO              POINT TO THE RECORD                          
         MVC   COMPKEY,0(RE)       SAVE THE TV KEY                              
         NI    2(RE),X'F0'                                                      
         OI    2(RE),X'03'         SET MEDIA TO NETWORK                         
         MVC   KEY(13),0(RE)       AND SAVE THE KEY                             
                                                                                
* NOW ADD THE NETWORK RECORD IF IT'S NOT ALREADY THERE                          
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST ALREADY THERE                           
         BNE   AAR4                                                             
         LA    R2,TRAMEDH                                                       
         LHI   R0,NETTHERE         TELL THEM NET CMML ALREADY THERE             
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
AAR4     GOTO1 ADDREC                                                           
*                                                                               
         LA    R2,TRAMEDH                                                       
         MVC   CONHEAD(31),=C'Records added for media T and N'                  
         OI    GENSTAT2,USMYOK     TELL GENCON USE MY MESSAGE                   
                                                                                
* REREAD TV RECORD                                                              
                                                                                
         MVC   KEY(13),COMPKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                WHERE DID IT GO?                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
AARX     DS    0H                  CODE DID NOT PROCESS MODE=XRECADD            
**NOP**  BAS   RE,GENR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*                                                                               
PUT      MVC   AIO,AIO3                                                         
         L     R2,AIO1                                                          
         MVC   KEY,0(R2)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
         CLC   0(256,R2),0(R4)     COMPARE START OF 2 RECS FOR CHANGE           
         BNE   PUT10                                                            
         CLC   =H'256',14(R2)      SEE IF REC LENGTH MORE THAN 256              
         BNL   PUT06                                                            
         LH    R3,14(,R2)          GET LENGTH                                   
         LA    R2,256(,R2)                                                      
         LA    R4,256(,R4)                                                      
         SH    R3,=H'256'                                                       
         LR    R5,R3                                                            
         CLCL  R2,R4                                                            
         BNE   PUT10                                                            
PUT06    MVI   IOOPT,C'Y'                                                       
         B     PUT12                                                            
PUT10    BAS   RE,GENR             GO GENERATE AUTO-TURNAROUND REQ              
PUT12    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO                                                           
         USING TXTKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
*                                                                               
         CLC   TXTKCLT,SPACES                                                   
         BH    DK05                                                             
         MVC   TRACLT,SPACES                                                    
         B     DK10                                                             
*                                                                               
DK05     GOTO1 CLUNPK,DMCB,TXTKCLT,QCLT                                         
*                                                                               
********************************88                                              
**********NOP  CODE BELOW                                                       
********************************88                                              
         B     DK08                                                             
*                                                                               
*NOP     TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
*....    BZ    DK08                 NO                                          
*                                                                               
         L     RE,AIO3             SAVE ANY EXISTING REC IN AIO3                
         LA    RF,2000                                                          
         L     R2,AIO1                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
*                                                                               
         L     RE,AIO1             RESTORE REC TO AIO1                          
         LA    RF,2000                                                          
         L     R2,AIO3                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
*                                                                               
         CLI   ERROR,0             ANY ERRORS                                   
         BE    DK06                 NO                                          
*                                                                               
         CLI   ERROR,SECLOCK       VALID ERR SECURITY LOCK-OUT                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,TRACLTH                                                       
*                                                                               
DK06     OC    TXTKPROD,TXTKPROD   ANY PRODUCT                                  
         BZ    DK08                 NO                                          
*                                                                               
* CHECK OUT BRAND LEVEL SECURITY                                                
*                                                                               
DK07     XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'TXTKPROD),TXTKPROD PRODUCT                                 
*                                                                               
         L     RE,AIO3             SAVE ANY EXISTING REC IN AIO3                
         LA    RF,2000                                                          
         L     R2,AIO1                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
*                                                                               
         LA    R2,FLDH                                                          
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         L     RE,AIO1             RESTORE REC TO AIO1 AGAIN                    
         LA    RF,2000                                                          
         L     R2,AIO3                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
*                                                                               
         CLI   ERROR,0             ANY ERROR                                    
         BE    DK08                                                             
*                                                                               
         CLI   ERROR,SECLOCK       VALID ERR SECURITY LOCK-OUT                  
         BE    TRAPERR                                                          
         DC    H'0'                                                             
*                                                                               
DK08     DS    0H                                                               
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
DK10     OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         XC    WORK(L'TRAPRD),WORK                                              
         MVC   WORK(L'TXTKPROD),TXTKPROD                                        
         CLC   TRAPRD,WORK                                                      
         BE    DK20                                                             
         MVC   TRAPRD,WORK         MOVE IN PROD                                 
         OI    TRAPRDH+6,X'80'     SET ON TRANSMIT BIT                          
DK20     XC    WORK(L'TRATYPE),WORK                                             
         MVC   WORK(L'TXTKTYP),TXTKTYP                                          
         CLI   TXTKTYP,C'H'                                                     
         BE    DK22                                                             
         CLI   TXTKTYP,C'F'                                                     
         BNE   DK24                                                             
         MVI   WORK+1,C'N'                                                      
         B     DK24                                                             
DK22     MVI   WORK+1,C'L'                                                      
DK24     CLC   TRATYPE,WORK                                                     
         BE    DK30                                                             
         MVC   TRATYPE,WORK        MOVE IN TYPE                                 
         OI    TRATYPEH+6,X'80'     SET ON TRANSMIT BIT                         
DK30     MVC   BAGYMD,TXTKAM                                                    
         CLC   BCLT,TXTKCLT        IS CLIENT SAME                               
         BE    DK32                YES                                          
*                                                                               
         BRAS  RE,FCLT             GO GET CLIENT SVCLIST                        
         BNE   EXIT                                                             
*                                                                               
DK32     MVC   QPRD,TXTKPROD                                                    
         MVC   TYPE,TXTKTYP                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR22                NO, CONTINUE WITH READ SEQUENTIAL            
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         XC    RECCT,RECCT                                                      
* BUILD KEY, AND DO READHI                                                      
         LA    R4,KEY                                                           
         USING TXTKEY,R4                                                        
         MVC   TXTKID(2),=XL2'0A23'                                             
         MVC   TXTKAM,BAGYMD                                                    
         MVC   TXTKCLT,BCLT                                                     
         MVC   TXTKPROD,QPRD                                                    
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    *+10                NO, LEAVE OUT TYPE                           
         MVC   TXTKTYP,TYPE                                                     
*                                                                               
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
*                                                                               
         LA    R0,12               MAX KEY COMPARE (-1)                         
         LA    R1,KEY+12           START AT END OF CMLKCML                      
LR02     CLI   0(R1),0             NONZERO IS VALID COMPARAND                   
         BNE   LR04                FOUND END OF COMPARE KEY                     
         BCTR  R0,0                DECREMENT LENGTH                             
         BCT   R1,LR02                                                          
LR04     STC   R0,COMPKEYL         SAVE COMPARE LENGTH                          
         MVC   COMPKEY,KEY                                                      
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR30                                                             
         B     LR50                GO SEND NO SEL MSG                           
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   EXIT                YES                                          
         MVC   SVKEY(2),=XL2'0A23' TO INSURE                                    
         MVC   SVKEY+2(1),BAGYMD   BUILD KEY                                    
         CLC   SVKEY(3),KEY        ONLY WANTED KEYS ARE PASSED                  
         BL    EXIT                                                             
         BH    LR20                                                             
LR30     ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,LRCLC            SEE IF PAST KEY                              
         BNE   LR50                YES, ALL DONE                                
         CLI   TYPE,0              WAS TYPE ENTERED                             
         BE    LR34                NO                                           
         CLC   TYPE,KEY+8          ONLY LIST REQUESTED TYPE                     
         BNE   LR20                                                             
LR34     CLI   SPOTNETF,C'N'       IF NETWORK ONLY H ALLOWED                    
         BNE   LR36                                                             
         CLI   KEY+8,C'H'          ONLY LIST HEADINGS FOR NETWORK               
         BNE   LR20                                                             
LR36     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         EJECT                                                                  
* CHECK FILTERS HERE - IF NEEDED                                                
LR40     CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
LRCLC    CLC   COMPKEY(0),KEY                                                   
LR50     OC    RECCT,RECCT                                                      
         BNZ   EXIT                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    EXIT                GO FORMAT FOR OFFLINE REPORT                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
*                                                                               
* FORMAT ONLINE LIST                                                            
*                                                                               
LRL      MVC   LISTAR,SPACES                                                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    *+14                                                             
         CLC   BCLT,TXTKCLT        SAME CLIENT?                                 
         BE    *+8                                                              
         BRAS  RE,FCLT                                                          
         BNE   LR20                GET NEXT RECORD                              
*                                                                               
         GOTO1 CLUNPK,DMCB,TXTKCLT,LCLT                                         
         MVC   LPROD,TXTKPROD                                                   
         CLI   SPOTNETF,C'N'                                                    
         BE    LRL12                                                            
         MVC   LTYPE(1),TXTKTYP                                                 
         CLI   TXTKTYP,C'H'                                                     
         BE    LRL10                                                            
         CLI   TXTKTYP,C'F'                                                     
         BNE   LRL12                                                            
         MVI   LTYPE+1,C'N'                                                     
         B     LRL12                                                            
LRL10    MVI   LTYPE+1,C'L'                                                     
LRL12    L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TXTTXTEL,R6                                                      
         ZIC   R1,TXTTXTLN                                                      
         SH    R1,=H'4'            GET TEXT LEN-1                               
         EX    R1,LRLMVC                                                        
         LH    R1,RECCT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,RECCT                                                         
         GOTO1 LISTMON                                                          
         B     LR20                                                             
LRLMVC   MVC   LTEXT(0),TXTTXT                                                  
         EJECT                                                                  
* FORMAT OFFLINE REPORT                                                         
*                                                                               
LRR      CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   LRR04                  NO                                        
         MVI   ELCODE,C'1'         CK ACTIVITY ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LRR04                                                            
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(5,0),(3,WORK) GET TODAY'S DATE                      
         CLC   ACTVADDT,WORK       WAS ADD TODAY                                
         BNE   LRR02               NO, CK CHANGE                                
         MVC   P+10(7),=C'* ADD *'                                              
         B     LRR04                                                            
LRR02    CLC   ACTVCHDT,WORK       WAS CHANGE TODAY                             
         BNE   LRR04                                                            
         MVC   P+10(7),=C'* CHG *'                                              
LRR04    DS    0H                                                               
*NOP     MVC   PPROD,TXTKPROD                                                   
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    *+14                 YES                                         
         CLC   BCLT,TXTKCLT                                                     
         BE    LRR06                                                            
*                                                                               
         BRAS  RE,FCLT                                                          
         BNE   EXIT                                                             
*                                                                               
LRR06    MVC   PPROD,TXTKPROD                                                   
*                                                                               
         CLI   SPOTNETF,C'N'                                                    
         BE    LRR12                                                            
         MVC   PTYPE(1),TXTKTYP                                                 
         CLI   TXTKTYP,C'H'                                                     
         BE    LRR10                                                            
         CLI   TXTKTYP,C'F'                                                     
         BNE   LRR12                                                            
         MVI   PTYPE+1,C'N'                                                     
         B     LRR12                                                            
LRR10    MVI   PTYPE+1,C'L'                                                     
LRR12    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR20                                                            
         MVC   PBOX(2),=C'NO'                                                   
         TM    TXTBOX-TXTBOXEL(R6),X'80'                                        
         BZ    *+10                                                             
         MVC   PBOX,=C'YES'                                                     
         MVI   ELCODE,X'40'                                                     
         BAS   RE,FIRSTEL                                                       
         USING TXTTXTEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
LRR20    ZIC   R1,TXTTXTLN                                                      
         SH    R1,=H'4'            GET TEXT LEN-1                               
         EX    R1,LRRMVC                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BE    LRR20                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
LRRMVC   MVC   PTEXT(0),TXTTXT                                                  
*                                                                               
HIDETYPE DC    C'CLT PRD    TEXT'                                               
SHOWTYPE DC    C'CLT PRD T  TEXT'                                               
         EJECT                                                                  
* EXPLAIN REASON CODE FOR ACTION ACT                                            
*                                                                               
EXPR     XC    WORK,WORK                                                        
         CLI   CHREASON,C'T'                                                    
         BNE   EXPR10                                                           
         MVC   WORK(12),=CL12'TURN AROUND'                                      
         B     EXPR20                                                           
EXPR10   CLI   CHREASON,C'N'                                                    
         BNE   EXPR30                                                           
         MVC   WORK(12),=CL12'NORMAL MAINT'                                     
EXPR20   CLI   CHREASON+1,C'A'                                                  
         BNE   EXPR22                                                           
         MVC   WORK+13(3),=C'ADD'                                               
         B     EXIT                                                             
EXPR22   CLI   CHREASON+1,C'C'                                                  
         BNE   EXPR30                                                           
         MVC   WORK+13(6),=C'CHANGE'                                            
         B     EXIT                                                             
EXPR30   MVC   WORK(7),=C'UNKNOWN'                                              
         B     EXIT                                                             
         PRINT GEN                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*                                                                               
GENR     NTR1                                                                   
         CLI   SVPROF+9,C'Y'       AUTO TURNAROUND                              
         BE    GENR10              YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BNE   EXIT                NO                                           
GENR10   XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.TEX.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMED                                               
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLT                                               
         MVC   REQUEST+32(2),=C'.*'                                             
         CLI   SVPROF+9,C'D'       IF REQ IS DDS                                
         BE    *+10                BYPASS                                       
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         CLI   SPOTNETF,C'N'                                                    
         BNE   *+16                                                             
         MVC   H8+28(3),SPACES                                                  
         MVC   H7+27(4),SPACES                                                  
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   EXIT                   NO                                        
         MVC   H3+43(11),=C'TURN-AROUND'                                        
         B     EXIT                                                             
         EJECT                                                                  
*        ERROR ROUTINES                                                         
*                                                                               
INVPFKY  MVC   CONHEAD,ERRPFKY                                                  
         J     ERREXIT                                                          
*                                                                               
INVPOSN  MVC   CONHEAD,ERRPOSN                                                  
         J     ERREXIT                                                          
*                                                                               
HDNTER   MVC   CONHEAD(L'HDNTMSG),HDNTMSG                                       
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
TXTLENER MVI   ERROR,INVTXTLN      TEXT LINE TOO LONG                           
         J     TRAPERR                                                          
*                                                                               
FTNTER   MVI   ERROR,INVFNSZ       MORE THAN 3 LINES OF FOOTNOTE                
         J     TRAPERR                                                          
*                                                                               
INVBOXER MVI   ERROR,INVBOXCD      BOXES MUST BE Y OR N                         
         J     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING       NO DATA ENTERED, REQUIRED                    
         J     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ERRPFKY  DC    CL60'* ERROR * UNDEFINED FUNCTION KEY *'                         
ERRPOSN  DC   CL60'* ERROR * CURSOR MUST BE ON A TEXT LINE FOR PF3/4 *'         
HDNTMSG  DC    CL60'* ERROR * MORE THAN 7 LINES OF TEXT WITH BOXES=Y *'         
*                                                                               
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,45,C'TEXT LIST'                                               
         SSPEC H1,71,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,45,C'---------'                                               
         SSPEC H2,71,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,83,RUN                                                        
         SSPEC H4,71,REPORT                                                     
         SSPEC H5,71,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H7,18,C'PRODUCT'                                                 
         SSPEC H7,28,C'TYPE'                                                    
         SSPEC H7,35,C'BOX'                                                     
         SSPEC H7,40,C'TEXT'                                                    
         SSPEC H8,18,C'-------'                                                 
         SSPEC H8,35,C'---'                                                     
         SSPEC H8,40,C'----'                                                    
         SSPEC H8,29,C'H/F'                                                     
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING TXTKEY,R4                                                        
*                                                                               
FCLT10   DS    0H                                                               
         MVC   SVTXTPRD,TXTKPROD   SAVE PRODUCT                                 
         MVC   SVKEY,KEY                                                        
*                                                                               
         CLC   BCLT,TXTKCLT        SAME CLIENT                                  
         BE    FCLT12               YES, BYPASS VALIDATION OF CLIENT            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   BCLT,TXTKCLT                                                     
         DROP  R4                                                               
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
         L     R0,AIO2                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         MVC   ELEM(8),TRACLTH                                                  
         MVI   ELEM+5,3                                                         
         MVC   ELEM+8(3),QCLT                                                   
         LA    R2,ELEM                                                          
*                                                                               
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0            CLEAR                                        
*                                                                               
         L     R0,AIO1             RESTORE RECORD                               
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
FCLT12   CLI   ERROR,0             ANY ERRORS                                   
         BNE   FCLT15                                                           
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT30               NO                                          
         OC    SVTXTPRD,SVTXTPRD   ANY PRODUCT                                  
         BZ    FCLT30                                                           
         B     FCLT19                                                           
*                                                                               
FCLT15   CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT25                                                           
*                                                                               
FCLT18   OC    SVTXTPRD,SVTXTPRD   ANY PRODUCT                                  
         BNZ   FCLT19                                                           
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    FCLT30              OK TO DISPLAY                                
         B     FCLT22                                                           
*                                                                               
FCLT19   LA    R0,NCLSTSIZ         FIND PROD                                    
         L     R1,ASVNCLST                                                      
*                                                                               
FCLT20   CLC   SVTXTPRD,0(R1)                                                   
         BE    FCLT30                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,FCLT20                                                        
*                                                                               
         MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
*                                                                               
FCLT22   CLI   MODE,PRINTREP                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   FCLT23                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
         GOTO1 HIGH                DUMMY READ HI                                
*                                                                               
FCLT22C  GOTO1 SEQ                 FOR READ SEQ                                 
         OC    SVTXTPRD,SVTXTPRD   WAS THERE A PRODUCT IN PREV RECORD           
         BNZ   FCLT26                                                           
         CLC   KEY(8),KEYSAVE      SAME A/M/CLT AND STILL NO PROD               
         BE    FCLT22C             YES, GET NEXT RECORD                         
         B     FCLT26                                                           
*                                                                               
FCLT23   CLI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         BE    *+6                                                              
         DC    H'0'                SOME OTHER ERROR???                          
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BO    TRAPERR1                                                         
         DC    H'0'                                                             
*                                                                               
FCLT25   MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
*                                                                               
FCLT26   DS    0H                                                               
         ZIC   R1,COMPKEYL         GET COMPARE LENGTH                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   COMPKEY(0),KEY                                                   
         BNE   FCLTX                                                            
*                                                                               
* DO GETREC & THEN SAVE REC                                                     
*                                                                               
         GOTO1 GETREC                                                           
         B     FCLT10                                                           
*                                                                               
* MOVE REC BACK TO AIO1, DO GETPROF                                             
*                                                                               
FCLT30   DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF OFFICE FROM CLIENT RECORD                    
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT33                                                           
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    FCLT33                                                           
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
FCLT33   GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    FCLT40              BYPASS                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
FCLT40   MVC   AIO,AIO1                                                         
         CR    RB,RB               SET COND CODE OKAY                           
FCLTX    XIT1                                                                   
*                                                                               
TRAPERR1 DS    0H                                                               
         LA    R2,TRAPRDH          PRODUCT CODE                                 
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
*                                                                               
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                                                             
*                                                                               
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK07                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         JNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK07     CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK12                YES, NOT NEEDED                              
         CLI   36(RA),X'FE'        SPOT TRAFFIC TEXT SCREEN LOADED?             
         BE    VK11                 YES                                         
*                                                                               
VK10     MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0                                                          
         BE    VK11                                                             
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         JZ    TRAPERR              NO                                          
*                                                                               
         CLI   ERROR,SECLOCK       ONLY VALID ERROR SEC-LOCKOUT                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         JO    TRAPERR                                                          
*                                                                               
VK11     XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK11F                                                            
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK11F                                                            
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK11F    GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'3'         READ T3 PROFILE                              
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF,ELEM                                                    
*                                                                               
         MVI   WORK+3,C'3'         READ T3PROF+6 FOR MEDIA T                    
         MVI   WORK+6,C'T'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PROF+6(1),ELEM+6                                             
*                                                                               
VK12     LA    R2,TRAPRDH          PRODUCT CODE                                 
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                                                             
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
VK17     MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
*                                                                               
VK20     LA    R2,TRATYPEH         TYPE                                         
         XC    TYPE,TYPE                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK36                YES, NOT NEEDED                              
         CLI   SPOTNETF,C'N'       IF NETWORK ONLY H ALLOWED                    
         BNE   VK30                                                             
         MVI   TYPE,C'H'                                                        
         B     VK36                                                             
*                                                                               
VK30     GOTO1 ANY                                                              
         CLI   WORK,C'H'           HEADLINE                                     
         BE    VK34                                                             
         CLI   SPOTNETF,C'N'       IF NETWORK ONLY H ALLOWED                    
         BE    INVNCDER                                                         
         CLI   WORK,C'F'           FOOTNOTE                                     
         JNE   INVCDER                                                          
*                                                                               
VK34     MVC   TYPE,WORK                                                        
*                                                                               
VK36     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING TXTKEY,R4                                                        
         MVC   TXTKID,=XL2'0A23'                                                
         MVC   TXTKAM,BAGYMD                                                    
         MVC   TXTKCLT,BCLT        MOVE IN CLIENT                               
         MVC   TXTKPROD,QPRD       MOVE PRODUCT INTO KEY                        
         MVC   TXTKTYP,TYPE                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKX                                                              
         CLI   SPOTCAN,C'C'                                                     
         BNE   VKX                                                              
         CLI   SVT3PROF+6,C'Y'     AUTO ADD MEDIA N CMML?                       
         BNE   VKX                                                              
*                                                                               
         CLI   TRAMED,C'N'         ADDING MEDIA N DIRECTLY?                     
         BNE   VKX                                                              
*                                                                               
         MVC   WORK(13),KEY        SAVE KEY                                     
         NI    KEY+2,X'F1'         CHANGE MEDIA TO T                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IS MEDIA T REC THERE?                        
         BE    VK40                                                             
         LA    R2,TRAMEDH                                                       
         LHI   R0,NOTVCMML                                                      
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
VK40     MVC   KEY(13),WORK        RESTORE KEY                                  
         DROP  R4                                                               
*                                                                               
VKX      J     EXIT                                                             
*                                                                               
INVCDER  MVI   ERROR,INVHFCDE      MUST BE H(EADLINE) F(OOTNOTE)                
         J     TRAPERR                                                          
*                                                                               
INVNCDER MVC   CONHEAD,INVNCDMS                                                 
         J     ERREXIT                                                          
INVNCDMS DC    CL60'* ERROR * NO FOOTNOTES FOR NETWORK *'                       
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE SPTRTXT                                                        
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAFED                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* DSECT FOR OFFLINE PRINT LINE *                                                
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL19                                                             
PPROD    DS    CL3                                                              
         DS    CL6                                                              
PTYPE    DS    CL2                                                              
         DS    CL4                                                              
PBOX     DS    CL3                                                              
         DS    CL2                                                              
PTEXT    DS    CL60                                                             
*                                                                               
* DSECT FOR ONLINE LIST LINE *                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPROD    DS    CL3                                                              
         DS    CL1                                                              
LTYPE    DS    CL2                                                              
         DS    CL1                                                              
LTEXT    DS    CL60                                                             
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0F                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
SVTXTPRD DS    CL3                 SAVE TEXT RECORD PROD                        
RECCT    DS    H                                                                
MXTXTLNS DS    H                                                                
HOLDAIO  DS    CL4                                                              
TYPE     DS    CL1                                                              
COMPKEY  DS    CL13                COMPARE KEY FOR ONLINE LIST                  
COMPKEYL DS    CL1                                                              
BOXES    DS    CL1                 Y(ES) OR N(O)                                
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPTRA0E   05/12/14'                                      
         END                                                                    
