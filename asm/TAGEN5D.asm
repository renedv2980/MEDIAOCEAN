*          DATA SET TAGEN5D    AT LEVEL 001 AS OF 02/07/13                      
*PHASE T7025DA,*                                                                
         TITLE 'T7025D - PAYTYPE MAINTENANCE/LIST'                              
T7025D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7025D,R7,R6                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
                                                                                
         LA    RE,STATTAB                                                       
         ST    RE,ASTATTAB         SAVE (STATUS TABLE)                          
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE REC                                 
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST REC                                     
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   *+8                                                              
         BRAS  RE,DE                                                            
                                                                                
         CLI   MODE,XRECADD        RECORD ADDED                                 
         JNE   *+8                                                              
         BRAS  RE,XR                                                            
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+8                                                              
         BRAS  RE,XR                                                            
                                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JNE   XIT                                                              
                                                                                
         LA    RF,HOOK             SET A(HEADLINE HOOK)                         
         ST    RF,HEADHOOK                                                      
         LA    RF,MYSPECS          SET A(SPECS)                                 
         ST    RF,SPECS                                                         
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         BRAS  RE,LR               GO LIST THE RECORDS                          
                                                                                
                                                                                
         XIT1                                                                   
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,PTLSELH,H4-1                                
*****    GOTO1 (RF),(R1),SMTHEADH,SMTSELH,H7-5                                  
****     MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         J     XIT                                                              
         SPACE 2                                                                
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H8,1,C'--- ----        ------------------- '                     
         SSPEC H8,54,C'-----  -----  ---- ------'                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ERINVD   STC   R3,ERRDISP          INVALID INPUT (WITH DISPLACEMENT)            
         J     ERINV                                                            
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNDEL   MVI   ERROR,ERINVDEL      RECORD NOT AVAILBLE FOR DELETE               
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
STATTAB  DS    0XL12                                                            
******** DC    AL1(TAYDSACO),AL1(TAYDSTAX+TAYDSWAG+TAYDSACO+TAYDSDED)           
******** DC    CL10'AC'                                                         
         DC    AL1(TAYDSDED),AL1(TAYDSTAX+TAYDSWAG+TAYDSACO+TAYDSDED)           
         DC    CL10'DEDUCT'                                                     
         DC    X'FF'                                                            
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   INIT30                                                           
         LA    R2,PTLSELH                                                       
         LA    R3,PTLLSELH                                                      
INIT10   CLI   5(R2),0                                                          
         JE    INIT20                                                           
         OC    8(L'PTLSEL,R2),SPACES                                            
         CLC   =C'S  ',8(R2)                                                    
         JE    INIT20                                                           
         CLC   =C'C  ',8(R2)                                                    
         JE    INIT20                                                           
         CLC   =C'DE ',8(R2)                                                    
         JNE   ERINV                                                            
INIT20   ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         JL    INIT10                                                           
                                                                                
INIT30   GOTO1 INITIAL,DMCB,0                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VKMAINT                                                       
         BAS   RE,VKLIST                                                        
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VKMAINT  NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JE    XIT                                                              
         LA    R2,PTYPTYH                                                       
         CLI   5(R2),3                                                          
         JL    ERINV                                                            
         CLI   8(R2),C' '                                                       
         JE    ERINV                                                            
         GOTO1 RECVAL,DMCB,TLPMCDQ,(X'40',(R2)),('TLPMSCDQ',0)                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR LIST SCREEN                      *         
***********************************************************************         
                                                                                
VKLIST   NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   XIT                                                              
                                                                                
                                                                                
         LHI   RF,TIEND-TASYSIOD                                                
         XCEFL TASYSIOD,(RF)                                                    
                                                                                
         BAS   RE,VKFMT                                                         
         BAS   RE,VKSTR                                                         
                                                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
                                                                                
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO VALIDATE SORT BY/FORMAT                           *         
***********************************************************************         
                                                                                
VKFMT    NTR1                                                                   
         MVI   TIREAD,TLPMNCDQ                                                  
         CLI   PTLFMT,C'N'                                                      
         JE    XIT                                                              
         CLI   PTLFMTH+5,0                                                      
         JNE   VKF10                                                            
         MVI   PTLFMT,C'N'                                                      
         MVI   PTLFMTH+5,1                                                      
         MVI   PTLFMTH+6,X'80'                                                  
         J     XIT                                                              
                                                                                
VKF10    MVI   TIREAD,TLPMCDQ                                                   
         MVI   TIRDSUBT,TLPMSCDQ                                                
         CLI   PTLFMT,C'C'                                                      
         JE    XIT                                                              
         LA    R2,PTLFMTH                                                       
         J     ERINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE START                                    *         
***********************************************************************         
                                                                                
VKSTR    NTR1                                                                   
         CLI   PTLSTRH+5,0                                                      
         JE    XIT                                                              
         MVC   TIQSTART(L'PTLSTR),PTLSTR                                        
         OC    TIQSTART,SPACES                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
***********************************************************************         
                                                                                
         USING TLPMD,R4                                                         
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVC   PTYPTY,TLPMPTYP                                                  
         MVI   PTYPTYH+5,L'TLPMPTYP                                             
         OI    PTYPTYH+6,X'80'                                                  
         DROP  R4                                                               
         BRAS  RE,VK                                                            
         J     XIT                                                              
*                                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY PAY TYPE RECORD)                          
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         GOTO1 FLDVAL,DMCB,(1,PTYNAMEH),(X'80',PTYSTATH)                        
                                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,PTYNAMEH   DISPLAY LONG NAME                
         MVI   ELCODE,TAYDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DR20                                                             
         USING TAYDD,R4                                                         
         MVI   PTYUNIT,C'N'                                                     
         MVI   PTYUNITH+5,1                                                     
         TM    TAYDSTAT,TAYDSUNT         UNIT BASED?                            
         BNO   DR10                                                             
         MVI   PTYUNIT,C'Y'                                                     
                                                                                
DR10     MVI   PTYWAGE,C'N'                                                     
         MVI   PTYWAGEH+5,1                                                     
         TM    TAYDSTAT,TAYDSWAG         WAGE?                                  
         BNO   DR20                                                             
         MVI   PTYWAGE,C'Y'                                                     
                                                                                
DR20     MVI   PTYTAX,C'N'                                                      
         MVI   PTYTAXH+5,1                                                      
         TM    TAYDSTAT,TAYDSTAX         TAXABLE?                               
         BNO   DR30                                                             
         MVI   PTYTAX,C'Y'                                                      
                                                                                
         USING STATTABD,RF                                                      
DR30     LA    R2,PTYSTAT                                                       
         L     RF,ASTATTAB                                                      
DR40     CLI   0(RF),X'FF'         DISPLAY STATUSES                             
         JE    DR60                                                             
         MVC   BYTE,STSTAT                                                      
         NC    BYTE,TAYDSTAT                                                    
         JZ    DR50                                                             
         CLI   0(R2),C' '                                                       
         JNH   *+12                                                             
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         MVC   0(10,R2),STLIT                                                   
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         BCT   R2,*-8                                                           
DR50     LA    RF,STLNQ(RF)                                                     
         J     DR40                                                             
         DROP  R4,RF                                                            
                                                                                
DR60     GOTO1 ACTVOUT,DMCB,PTYLCHGH     LAST CHANGED ACTIVITY                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
*        ON ENTRY ... AIO = A(PRIMARY PAY TYPE RECORD)                          
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SAVPTRS,DMCB,SVPTRS                                              
                                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,(X'20',PTYNAMEH)                              
                                                                                
         MVI   ELCODE,TAYDELQ           BUILD PAYMENT TYPE DETAILS ELEM         
         GOTO1 REMELEM                                                          
                                                                                
         USING TAYDD,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAYDEL,TAYDELQ                                                   
         MVI   TAYDLEN,TAYDLNQ                                                  
                                                                                
         LA    R2,PTYUNITH                                                      
         GOTO1 ANY                      UNIT BASED?                             
         CLI   8(R2),C'N'                                                       
         JE    VR20                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   ERINV                                                            
         OI    TAYDSTAT,TAYDSUNT                                                
                                                                                
VR20     LA    R2,PTYWAGEH                                                      
         GOTO1 ANY                      WAGE?                                   
         CLI   8(R2),C'N'                                                       
         JE    VR30                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   ERINV                                                            
         OI    TAYDSTAT,TAYDSWAG+TAYDSTAX                                       
                                                                                
         CLI   PTYTAXH+5,0              IF WAGE BASED, MUST                     
         JE    VR40                     BE TAXABLE                              
         CLI   PTYTAX,C'Y'                                                      
         JE    VR40                                                             
         LA    R2,PTYTAXH                                                       
         J     ERINV                                                            
                                                                                
VR30     LA    R2,PTYTAXH                                                       
         GOTO1 ANY                      IF NOT WAGE BASED, CAN                  
         CLI   8(R2),C'N'               BE TAXABLE OR NON-TAXABLE               
         JE    VR40                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   ERINV                                                            
         OI    TAYDSTAT,TAYDSTAX                                                
                                                                                
VR40     CLI   PTYSTATH+5,0                                                     
         JE    VR80                                                             
         LA    R2,PTYSTATH                                                      
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         JE    ERINV                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
         USING SCAND,R5                                                         
         LA    R5,BLOCK                                                         
         SR    R3,R3                                                            
                                                                                
         USING STATTABD,RF                                                      
VR50     L     RF,ASTATTAB                                                      
                                                                                
VR60     CLI   0(RF),X'FF'                                                      
         JE    ERINVD                                                           
         ZIC   R1,SCLEN1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   SCDATA1(0),STLIT                                                 
         JE    VR70                                                             
         LA    RF,STLNQ(RF)                                                     
         J     VR60                                                             
                                                                                
VR70     CLI   SCLEN2,0                                                         
         JNE   ERINVD                                                           
                                                                                
         MVC   BYTE,STINVW                                                      
         NC    BYTE,TAYDSTAT                                                    
         JNZ   ERINVD                                                           
                                                                                
         OC    TAYDSTAT,STSTAT                                                  
                                                                                
         ZIC   RF,SCLEN1                                                        
         LA    R3,1(R3,RF)                                                      
         LA    R5,SCANNEXT                                                      
         BCT   R0,VR50                                                          
         DROP  RF                                                               
                                                                                
VR80     GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ACTVIN,DMCB,0            UPDATE LAST CHANGED ELEMENT             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DELETE THE RECORD                                 *         
*                                                                               
***********************************************************************         
                                                                                
DE       NTR1  BASE=*,LABEL=*                                                   
         USING TLPMD,R4                                                         
         L     R4,AIO                                                           
         TM    TLPMSTAT,TLPMSUSD                                                
         JO    ERNDEL                                                           
         DROP  R4                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,SVPTRS                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS POST-ADD AMD POST CHANGE                  *         
***********************************************************************         
                                                                                
XR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ADDPTRS,DMCB,SVPTRS                                              
         CLI   MODE,XRECDEL                                                     
         JE    XIT                                                              
         BRAS  RE,DR                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         MVC   P,SPACES                                                         
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         LA    R2,P                R2=A(DISPLAY LINE)                           
         AHI   R2,L'LSELECT                                                     
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,15                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         OC    COUNTER,COUNTER     IF ANYTHING REPORTED                         
         BZ    LRX                                                              
         BRAS  RE,PRNTIT           SKIP A LINE                                  
***      BRAS  RE,PRNTIT           SKIP A LINE                                  
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         GOTO1 FLDVAL,DMCB,(X'01',PTLLSTH),(X'80',PTLLAST)                      
         B     LRX                                                              
LRX      J     XIT                                                              
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         USING LISTD,R2                                                         
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
                                                                                
         MVC   AIO,TIAREC                                                       
         L     R4,AIO                                                           
         USING TLPMD,R4                                                         
         CLI   TLPMCD,TLPMCDQ                                                   
         JNE   YES                                                              
         CLI   TLPMSCD,TLPMSCDQ                                                 
         JNE   YES                                                              
         MVC   LTYPE(L'TLPMPTYP),TLPMPTYP                                       
                                                                                
         MVI   DUMNAMH,L'DUMNAMH                                                
         MVC   DUMNAMH+8(L'DUMNAMH-8),SPACES                                    
         GOTO1 CHAROUT,DMCB,TANAELQ,DUMNAMH                                     
         MVC   LNAM,DUMNAMH+8                                                   
                                                                                
         USING TAYDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAYDELQ        PAYMENT TYPE DETAILS ELEM                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   LUNIT,C'N'                                                       
         TM    TAYDSTAT,TAYDSUNT                                                
         JZ    *+8                                                              
         MVI   LUNIT,C'Y'                                                       
                                                                                
         MVI   LWAGE,C'N'                                                       
         TM    TAYDSTAT,TAYDSWAG                                                
         JZ    *+8                                                              
         MVI   LWAGE,C'Y'                                                       
                                                                                
         MVI   LTAX,C'N'                                                        
         TM    TAYDSTAT,TAYDSTAX                                                
         JZ    *+8                                                              
         MVI   LTAX,C'Y'                                                        
                                                                                
         MVI   LSTAT,C' '                                                       
         TM    TAYDSTAT,TAYDSSUP+TAYDSACO+TAYDSDED                              
         JZ    *+8                                                              
         MVI   LSTAT,C'*'                                                       
         DROP  R4                                                               
                                                                                
DISP40   CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    DISP50                                                           
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON                                                          
         J     YES                                                              
DISP50   GOTO1 SPOOL,DMCB,(R8)                                                  
         LH    RE,COUNTER          DISPLAY LINE TO REPORT                       
         AHI   RE,1                                                             
         STH   RE,COUNTER                                                       
LRHX     J     YES                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR67D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR68D                                                       
                                                                                
         DS    D                                                                
ASTATTAB DS    A                   A(STATUS TABLE)                              
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
SVPTRS   DS    CL(L'TLDRREC*2+1)                                                
DUMNAMH  DS    CL(L'LNAM+8)                                                     
COUNTER  DS    H                                                                
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT TO COVER LIST LINE                                    *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
LSELECT  DS    0CL4                                                             
LTYPE    DS    CL11                TYPE                                         
         DS    CL1                                                              
LNAM     DS    CL36                NAME                                         
         DS    CL3                                                              
LUNIT    DS    CL1                 UNIT-BASED?                                  
         DS    CL6                                                              
LWAGE    DS    CL1                 WAGE?                                        
         DS    CL5                                                              
LTAX     DS    CL1                 TAX?                                         
         DS    CL5                                                              
LSTAT    DS    CL1                 STATUS                                       
                                                                                
**********************************************************************          
*        DSECT TO STATUS TABLE                                       *          
**********************************************************************          
                                                                                
STATTABD DSECT                                                                  
STSTAT   DS    XL1                                                              
STINVW   DS    XL1                                                              
STLIT    DS    CL10                                                             
STLNQ    EQU   *-STATTABD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAGEN5D   02/07/13'                                      
         END                                                                    
