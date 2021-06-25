*          DATA SET TAGEN57    AT LEVEL 002 AS OF 11/30/12                      
*PHASE T70257E,*                                                                
         TITLE 'T7027D - TASK MAINTENANCE/LIST'                                 
T70257   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70257,R7,R6                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
                                                                                
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,VALREC         VALREC                                       
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST REC                                     
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
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
         XC    COUNTER,COUNTER     CLEAR LINE COUNTER                           
         SPACE 1                                                                
         BRAS  RE,LR               GO LIST THE RECORDS                          
                                                                                
         XIT1                                                                   
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,TSLFRSTH,H4-1                               
***      GOTO1 (RF),(R1),TSLFRSTH,TSLSELH,H7-5                                  
***      MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
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
         SSPEC H8,1,C'--- -------     --------------------------------'         
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
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   INIT30                                                           
         LA    R2,TSLSELH                                                       
         LA    R3,TSLLSELH                                                      
INIT10   CLI   5(R2),0                                                          
         JE    INIT20                                                           
         OC    8(L'TSLSEL,R2),SPACES                                            
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
         LA    R2,TSKTASKH                                                      
         CLI   5(R2),3                                                          
         JL    ERINV                                                            
         CLI   8(R2),C' '                                                       
         JE    ERINV                                                            
         GOTO1 RECVAL,DMCB,TLTKCDQ,(X'40',(R2)),('TLTKSCDQ',0)                  
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
         MVI   TIREAD,TLTKNCDQ                                                  
         CLI   TSLFMT,C'N'                                                      
         JE    XIT                                                              
         CLI   TSLFMTH+5,0                                                      
         JNE   VKF10                                                            
         MVI   TSLFMT,C'N'                                                      
         MVI   TSLFMTH+5,1                                                      
         MVI   TSLFMTH+6,X'80'                                                  
         J     XIT                                                              
                                                                                
VKF10    MVI   TIREAD,TLTKCDQ                                                   
         MVI   TIRDSUBT,TLTKSCDQ                                                
         CLI   TSLFMT,C'C'                                                      
         JE    XIT                                                              
         LA    R2,TSLFMTH                                                       
         J     ERINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE START                                    *         
***********************************************************************         
                                                                                
VKSTR    NTR1                                                                   
         CLI   TSLSTRH+5,0                                                      
         JE    XIT                                                              
         MVC   TIQSTART(L'TSLSTR),TSLSTR                                        
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
                                                                                
         USING TLTKD,R4                                                         
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVC   TSKTASK,TLTKTASK                                                 
         MVI   TSKTASKH+5,L'TLTKTASK                                            
         OI    TSKTASKH+6,X'80'                                                 
         DROP  R4                                                               
                                                                                
                                                                                
         BRAS  RE,VK                                                            
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
*        ON ENTRY ... AIO = A(PRIMARY TASK RECORD)                              
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SAVPTRS,DMCB,SVPTRS                                              
                                                                                
*                                                                               
         GOTO1 NAMIN,DMCB,TANAELQ,(X'20',TSKNAMEH)                              
                                                                                
VR10     GOTO1 ACTVIN,DMCB,TSKLCHG      UPDATE LAST CHANGED ELEMENT             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO DELETE THE RECORD                                 *         
*                                                                               
***********************************************************************         
DE       NTR1  BASE=*,LABEL=*                                                   
         USING TLTKD,R4                                                         
         L     R4,AIO                                                           
         TM    TLTKSTAT,TLTKSUSD                                                
         JO    ERNDEL                                                           
         DROP  R4                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,SVPTRS                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS POST-ADD AMD POST CHANGE                  *         
*                                                                               
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
         MVC   P,SPACES       CLEAR PREVIOUS LINE                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         LA    R2,P                R2=A(DISPLAY LINE)                           
         AHI   R2,L'LSELECT                                                     
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         OC    COUNTER,COUNTER     IF ANYTHING REPORTED                         
         BZ    LRX                                                              
         BRAS  RE,PRNTIT           SKIP A LINE                                  
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         GOTO1 FLDVAL,DMCB,(X'01',TSLLSTH),(X'80',TSLLAST)                      
         B     LRX                                                              
LRX      J     XIT                                                              
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         USING LISTD,R2                                                         
         MVC   P,SPACES                                                         
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
*                                                                               
*                                                                               
         MVC   AIO,TIAREC                                                       
         L     R4,AIO                                                           
         USING TLTKD,R4                                                         
         CLI   TLTKCD,TLTKCDQ                                                   
         JNE   YES                                                              
         CLI   TLTKSCD,TLTKSCDQ                                                 
         JNE   YES                                                              
         MVC   LCODE,TLTKTASK                                                   
*                                                                               
         MVI   DUMNAMH,L'DUMNAMH                                                
         MVC   DUMNAMH+8(L'DUMNAMH-8),SPACES                                    
         GOTO1 CHAROUT,DMCB,TANAELQ,DUMNAMH                                     
         MVC   LNAM,DUMNAMH+8                                                   
*                                                                               
DISP40   CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    DISP50                                                           
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON                                                          
         J     LRHX                                                             
                                                                                
DISP50   GOTO1 SPOOL,DMCB,(R8)                                                  
         LH    RE,COUNTER          DISPLAY LINE TO REPORT                       
         AHI   RE,1                                                             
         STH   RE,COUNTER                                                       
LRHX     J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ...   AIO=A(TASK RECORD)                                      
***********************************************************************         
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(1,TSKNAMEH),(X'80',TSKNAMEH)                        
                                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,TSKNAMEH   DISPLAY LONG NAME                
*                                                                               
DR20     GOTO1 ACTVOUT,DMCB,TSKLCHGH     LAST CHANGED ACTIVITY                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6DD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6ED                                                       
                                                                                
         DS    D                                                                
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
                                                                                
LISTD   DSECT                                                                   
LSELECT  DS    0CL4                PRINT REPORT SELECT FIELD                    
LCODE    DS    CL6                 CODE                                         
         DS    CL6                                                              
LNAM     DS    CL36                NAME                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGEN57   11/30/12'                                      
         END                                                                    
