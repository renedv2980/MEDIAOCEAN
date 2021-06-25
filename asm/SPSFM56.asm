*          DATA SET SPSFM56    AT LEVEL 010 AS OF 02/19/09                      
*PHASE T21756A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21756  -- SQAD DAYPARTS                             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SQAD RECORDS                               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21900), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM60 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- RECORD                                         *         
*                R4 -- TABLE                                          *         
*                R5 -- WORK                                           *         
*                R6 -- ELEMENT                                        *         
*                R7 -- WORK                                           *         
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
         TITLE 'T21756 - SQAD MAINTENANCE'                                      
T21756   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1756**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORD                                  
         BE    LR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       MVC   SQDMEDN,SPACES        CLEAR MEDIA NAME FROM SCREEN               
         OI    SQDMEDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,SQDMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE                        
*                                                                               
         CLI   SVAPROF+7,C'C'        SQAD RECORDS INVALID IN CANADA             
         BNE   VK10                                                             
         LA    R2,CONRECH                                                       
         B     ERRINV                                                           
*                                                                               
VK10     MVC   SQDMEDN,MEDNM         TRASMIT MEDIA NAME                         
         OI    SQDMEDNH+6,X'80'                                                 
*                                                                               
VK20     LA    R2,SQDDPRTH           VALIDATE DAYPART MENU                      
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTEQU,ACTLIST                                                   
         BNE   ERRMIS                                                           
         B     VK40                                                             
*                                                                               
VK30     XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DPTHDR,R3                                                        
*                                                                               
         MVI   DPTKTYPE,X'08'                                                   
         MVC   DPTKAGY,AGENCY                                                   
         MVC   DPTKMED,SQDMEDK                                                  
         MVC   DPTKMENU,SQDDPRT                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(5),KEY                                                   
         BNE   ERRINV                                                           
*                                                                               
***********************************************************************         
*                                                                               
VK40     XC    KEY,KEY                                                          
         LA    R3,KEY                KEY FIELDS HAVE BEEN VALIDATED             
         USING SQDRECD,R3            NOW BUILD KEY ...                          
         XC    KEY,KEY                                                          
         MVI   SQDKTYP,SQDKTYPQ      RECORD TYPE X'0D3B'                        
         MVI   SQDKSUB,SQDKSUBQ                                                 
         MVC   SQDKAGMD,BAGYMD       MEDIA                                      
         MVC   SQDKDPT,SQDDPRT       DAYPART MENU                               
         MVC   SQADKEY,KEY           SAVE AS SQADKEY                            
         MVC   SVSQDKEY,KEY          SAVE A COPY FOR LIST                       
VKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
         USING SQDRECD,R3                                                       
VR       L     R3,AIO                SQAD RECORD AT AIO                         
*                                                                               
         CLI   ACTEQU,ACTADD         IF ACTION CHANGE, DELETE ALL OF            
         BE    VR10                  THE RECORD'S ELEMENTS                      
*                                                                               
         MVI   ELCODE,SQDELQ                                                    
         GOTO1 REMELEM                                                          
*                                                                               
***********************************************************************         
VR10     LA    R2,SQDDAY1H           INITIALIZE TO FIRST FIELD                  
         LA    R5,SQDDAY2H           STAYS POINTED AT LAST FIELD                
*                                                                               
VR20     CLI   5(R2),0                                                          
         BE    VR80                  IF DAYPART CODE ENTERED IN CURRENT         
*                                                                               
         CLI   QMED,C'R'                                                        
         BE    VR50                                                             
*                                                                               
         CLI   5(R2),2               TV DAYPART CODES MUST BE 2 CHARS           
         BNE   ERRINV                                                           
*                                                                               
         USING TABD,R4               FIELD ...                                  
         LA    R4,TAB                                                           
VR30     CLI   0(R4),X'FF'           INPUTTED DAYPART CODE MUST BE IN           
         BE    ERRINV                TABLE OR ERROR                             
         CLC   8(2,R2),TABCODE                                                  
         BE    VR40                                                             
         LA    R4,TABLQ(R4)                                                     
         B     VR30                                                             
         DROP  R4                                                               
*                                                                               
         USING SQDEL,R6              IF INPUTTED DAYPART CODE VALID ...         
VR40     LA    R6,ELEM               BUILD AND ADD ELEMENT                      
         XC    ELEM,ELEM                                                        
         MVI   SQDEL,SQDELQ          DAYPART ELEMENT CODE                       
         MVI   SQDLEN,SQDLENQ        DAYPART ELEMENT LENGTH                     
         LR    RE,R2                                                            
         BCTR  RE,0                                                             
         MVC   SQDDPT,0(RE)          DAYPART MENU CODE                          
         MVC   SQDSQAD,8(R2)         SQAD DAYPART CODE                          
         GOTO1 ADDELEM                                                          
         B     VR80                  SKIP RADIO PROCESSING                      
         DROP  R6                                                               
*                                                                               
* THIS IS FOR RADIO ONLY!                                                       
*                                                                               
         USING TAB2D,R4              FIELD ...                                  
VR50     LA    R4,TAB2                                                          
VR60     CLI   0(R4),X'FF'           INPUTTED DAYPART CODE MUST BE IN           
         BE    ERRINV                TABLE OR ERROR                             
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),TAB25COD                                                 
         BE    VR70                                                             
         LA    R4,TAB2LQ(R4)                                                    
         B     VR60                                                             
*                                                                               
         USING SQDEL,R6              IF INPUTTED DAYPART CODE VALID ...         
VR70     LA    R6,ELEM               BUILD AND ADD ELEMENT                      
         XC    ELEM,ELEM                                                        
         MVI   SQDEL,SQDELQ          DAYPART ELEMENT CODE                       
         MVI   SQDLEN,SQDLENQ        DAYPART ELEMENT LENGTH                     
         LR    RE,R2                                                            
         BCTR  RE,0                                                             
         MVC   SQDDPT,0(RE)          DAYPART MENU CODE                          
         MVC   SQDSQAD,TAB22COD      SQAD DAYPART CODE                          
         OC    SQDSQAD,SPACES        SQAD DAYPART CODE                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         DROP  R4                                                               
*                                                                               
VR80     ZIC   R0,0(R2)                                                         
         AR    R2,R0                  BUMP TO NEXT UNPROTECTED FIELD            
         CR    R2,R5                                                            
         BNL   VRX                                                              
         TM    1(R2),X'20'                                                      
         BO    VR80                                                             
         B     VR20                                                             
*                                                                               
VRX      B     DR                    REDISPLAY RECORD                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       LA    R2,SQDDAY1H          INITIALIZE TO FIRST FIELD                   
         LA    R0,SQDDAY2H          STAYS POINTED AT LAST FIELD                 
*                                                                               
DR10     ZIC   R1,0(R2)             LENGTH OF CURRENT FIELD                     
         SH    R1,=H'9'             MINUS HEADER AND 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES       BLANK CURRENT FIELD AND TRANSMIT            
         OI    6(R2),X'80'                                                      
*                                                                               
DR20     ZIC   R1,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R1                                                            
         CR    R2,R0                END OF SCREEN?                              
         BH    DR25                                                             
         CLI   0(R2),20             CLEAR ALL UNPROTECTED AND 12 BYTE           
         BE    DR10                 PROTECTED FIELDS                            
         TM    1(R2),X'20'                                                      
         BZ    DR10                                                             
         B     DR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
         USING SQDRECD,R3                                                       
DR25     DS    0H                                                               
         L     R6,AIO               GET FIRST DAYPART ELEMENT                   
         MVI   ELCODE,SQDELQ                                                    
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         CLI   QMED,C'R'                                                        
         BE    DR50                                                             
*                                                                               
         USING SQDEL,R6                                                         
*                                                                               
         LA    R5,SQDDAY2H           STAYS POINTED AT LAST FIELD                
DR30     LA    R2,SQDDAY1H           INITIALIZE TO FIRST FIELD                  
*                                                                               
DR35     LR    RE,R2                 IF DAYPART MENU CODE ON SCREEN             
         BCTR  RE,0                  MATCHES DAYPART MENU CODE FROM             
         CLC   0(1,RE),SQDDPT        ELEMENT ...                                
         BNE   DR40                                                             
         MVC   8(2,R2),SQDSQAD       MOVE ELEMENT'S DAYPART CODE TO             
         OI    6(R2),X'80'           SCREEN                                     
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                 BUMP TO DAYPART DESCRIPTION FIELD          
         LA    R4,TAB                                                           
         USING TABD,R4                                                          
DR37     CLC   SQDSQAD,TABCODE       FIND DAYPART CODE IN TABLE AND             
         BE    DR39                  MOVE MATCHING DAYPART DESCRIPTION          
         LA    R4,TABLQ(R4)        TO SCREEN                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR37                                                             
         DC    H'00'                                                            
DR39     MVC   8(L'TABNAME,R2),TABNAME                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTEL             GET NEXT DAYPART ELEMENT                   
         BNE   DRX                                                              
         B     DR30                                                             
*                                                                               
DR40     ZIC   RE,0(R2)              IF DAYPART MENU CODE ON SCREEN             
         AR    R2,RE                 DOES NOT MATCH DAYPART MENU CODE           
         CR    R2,R5                 FROM ELEMENT ...                           
         BNL   DR45                  BUMP TO NEXT UNPROTECTED FIELD             
         TM    1(R2),X'20'                                                      
         BO    DR40                  IF PASS END OF SCREEN ... DIE              
         B     DR35                  ERROR BUILDING RECORD                      
DR45     DC    H'00'                                                            
*                                                                               
DR50     LA    R5,SQDDAY2H           STAYS POINTED AT LAST FIELD                
DR60     LA    R2,SQDDAY1H           INITIALIZE TO FIRST FIELD                  
*                                                                               
DR65     LR    RE,R2                 IF DAYPART MENU CODE ON SCREEN             
         BCTR  RE,0                  MATCHES DAYPART MENU CODE FROM             
         CLC   0(1,RE),SQDDPT        ELEMENT ...                                
         BNE   DR70                                                             
*                                                                               
         LA    R4,TAB2                                                          
         USING TAB2D,R4                                                         
*                                                                               
DR67     CLC   SQDSQAD,TAB22COD      FIND DAYPART CODE IN TABLE AND             
         BE    DR69                  MOVE MATCHING DAYPART DESCRIPTION          
         LA    R4,TAB2LQ(R4)         TO SCREEN                                  
         CLI   0(R4),X'FF'                                                      
         BNE   DR67                                                             
         DC    H'00'                                                            
*                                                                               
DR69     MVC   8(5,R2),TAB25COD      MOVE ELEMENT'S DAYPART CODE TO             
         OI    6(R2),X'80'           SCREEN                                     
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                 BUMP TO DAYPART DESCRIPTION FIELD          
         MVC   8(L'TAB2NAME,R2),TAB2NAME                                        
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTEL             GET NEXT DAYPART ELEMENT                   
         BNE   DRX                                                              
         B     DR60                                                             
*                                                                               
DR70     ZIC   RE,0(R2)              IF DAYPART MENU CODE ON SCREEN             
         AR    R2,RE                 DOES NOT MATCH DAYPART MENU CODE           
         CR    R2,R5                 FROM ELEMENT ...                           
         BNL   DR75                  BUMP TO NEXT UNPROTECTED FIELD             
         TM    1(R2),X'20'                                                      
         BO    DR70                  IF PASS END OF SCREEN ... DIE              
         B     DR65                  ERROR BUILDING RECORD                      
DR75     DC    H'00'                                                            
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY(L'SVSQDKEY),SVSQDKEY                                         
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR30     CLC   KEY(3),SVSQDKEY     SAME RECORD TYPE STILL?                      
         BNE   LRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES                                                    
         L     R6,AIO                                                           
         USING SQDRECD,R6                                                       
*                                                                               
         MVC   BYTE,SQDKAGMD                                                    
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB                                                        
*                                                                               
LR40     CLC   BYTE,1(R5)                                                       
         BE    LR50                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   LR40                                                             
*                                                                               
LR50     MVC   LSTMED,0(R5)                                                     
         MVC   LSTDMEN,SQDKDPT                                                  
         GOTO1 LISTMON                                                          
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
***********************************************************************         
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING SQDRECD,R6                                                       
*                                                                               
         MVC   BYTE,SQDKAGMD       PULL THE MEDIA OUT                           
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB                                                        
*                                                                               
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   DK10                                                             
*                                                                               
DK20     MVC   SQLMED,0(R5)                                                     
         MVC   SQLDPT,SQDKDPT                                                   
         OI    SQLMEDH+6,X'80'                                                  
         OI    SQLDPTH+6,X'80'                                                  
DKX      B     XIT                                                              
                                                                                
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
SETUPX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
*                                                                               
ERRMIS   MVI   ERROR,MISSING                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GETEL                                                        *         
***********************************************************************         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
*        SQAD DAYPARTS TABLE                                          *         
* !IMPORTANT!  This table is also used by SPSYSDRV1                             
***********************************************************************         
*                                                                               
TAB      DC   CL2'EM',CL12'Early AM'                                            
         DC   CL2'DA',CL12'Day'                                                 
         DC   CL2'EF',CL12'Early Fringe'                                        
         DC   CL2'EN',CL12'Early News'                                          
         DC   CL2'PA',CL12'Prime Access'                                        
         DC   CL2'PR',CL12'Prime'                                               
         DC   CL2'LN',CL12'Late News'                                           
         DC   CL2'LF',CL12'Late Fringe'                                         
         DC   X'FF'                                                             
***********************************************************************         
*        SQAD RADIO DAYPARTS TABLE                                              
* !IMPORTANT!  This table is also used by SPSYSDRV1                             
***********************************************************************         
*                                                                               
TAB2     DC   CL5'AMDRV',CL12'Morning Drv',CL2'AM'                              
         DC   CL5'DAY  ',CL12'Daytime',CL2'DA'                                  
         DC   CL5'PMDRV',CL12'Afternoon Dr',CL2'PM'                             
         DC   CL5'EVE  ',CL12'Evening',CL2'EV'                                  
         DC   CL5'AVG  ',CL12'Average',CL2'AV'                                  
         DC   CL5'WE   ',CL12'Weekend',CL2'WE'                                  
         DC   CL5'TAP  ',CL12'Total AM-PM',CL2'AP'                              
         DC   CL5'MSAVG',CL12'Mon Sun Avg',CL2'MS'                              
         DC   X'FF'                                                             
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*                                                                               
PRODUCT DSECT                                                                   
       ++INCLUDE SPGENSQAD         SQAD RECORD                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENDAYPT        DAYPART RECORD                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM60D          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM64D          LISTREC SCREEN                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
SQADKEY  DS    CL13                                                             
SVSQDKEY DS    CL13                                                             
TEMPMED  DS    XL1                                                              
         EJECT                                                                  
***********************************************************************         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL2                                                              
LSTMED   DS    CL1                                                              
         DS    CL16                                                             
LSTDMEN  DS    CL1                                                              
***********************************************************************         
*        TAB DSECT                                                    *         
***********************************************************************         
*                                                                               
TABD     DSECT                                                                  
TABCODE  DS    CL2                                                              
TABNAME  DS    CL12                                                             
TABLQ    EQU   *-TABD                                                           
*                                                                               
***********************************************************************         
*        TAB2 DSECT                                                             
***********************************************************************         
*                                                                               
TAB2D    DSECT                                                                  
TAB25COD DS    CL5                                                              
TAB2NAME DS    CL12                                                             
TAB22COD DS    CL2                                                              
TAB2LQ   EQU   *-TAB2D                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPSFM56   02/19/09'                                      
         END                                                                    
