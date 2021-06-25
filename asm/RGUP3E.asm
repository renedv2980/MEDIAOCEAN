*          DATA SET RGUP3E     AT LEVEL 142 AS OF 11/08/99                      
*PHASE TA0A3E                                                                   
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM3E -- ID RECORD MAINTENANCE/LIST/REPORT         *         
*                                                                     *         
*  COMMENTS:     MAINTAINS STAFF TYPE RECORDS ON GENDIR/GENFIL        *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMCE (MAINTENANCE)                        *         
*                        CTSFMBE (LIST)                               *         
*                        CTSFMAE (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED LOTUS ID RECORDS, LIST, OR REPORT.           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A3E - OFFICE ID DATATYPE RECORD MAINT/LIST/REPORT'           
TA0A3E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A3E**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*        CLI   MODE,LISTRECS       LIST RECORDS                                 
*        BE    LR                                                               
*        CLI   MODE,PRINTREP       PRINT RECORDS                                
*        BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                     VALIDATE KEY                          *                   
*************************************************************                   
*                                                                               
* VK       CLI   ACTEQU,ACTREP                                                  
*        BE    VKX                                                              
*        CLI   ACTEQU,ACTLIST                                                   
*        BE    VKX                                                              
*                                                                               
VK       XC    USERID,USERID       READY TO STORE THE USERID IN TWA             
         LA    R2,TR2UIDH          POINT R2 TO THE ID FIELD                     
         CLI   TR2UIDH+5,0         ANY DATA IN THIS FIELD?                      
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   TR2UIDH+5,8          IS THE DATA A LENGTH OF 8?                  
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         TM    TR2UIDH+4,X'04'      IS THE FIELD ALPHABETIC                     
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         CLC   TR2UID+4(2),=C'DD'    CHK FOR DD IN KEY                          
         BE    STORKEY                                                          
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
STORKEY  MVC   USERID,TR2UID                                                    
         B     VK20                                                             
*                                                                               
VK20     LA    R4,KEY              BUILD THE KEY                                
         USING OINFKEYD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   OINFSYS,OINFSYSQ                                                 
         MVI   OINFTYP,OINFTYPQ                                                 
         MVC   OINFSTAF,USERID                                                  
         MVC   KEY2,KEY            SAVE THE KEY                                 
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
*************************************************************                   
*                      VALIDATE RECORD                      *                   
*************************************************************                   
VR       L     R4,AIO              ADDRESS OF AIO1                              
         USING OINFKEY,R4          MASK WITH DSECT OF RECORD TO ADDELEM         
         MVC   USERID,OINFSTAF                                                  
                                                                                
         MVI   ELCODE,OINAMELQ     FIRST ADD NAME ELEMENT                       
         GOTO1 REMELEM             CLEAR IT                                     
         LA    R6,ELEM                                                          
         USING OINAMD,R6           NAME ELEMENT DSECT                           
         XC    ELEM,ELEM                                                        
         MVI   OINAMEL,OINAMELQ    ELCODE                                       
         MVI   OINAMLN,OINAMLNQ    EL LENGTH                                    
         LA    R2,TR2NAMEH         INPUT                                        
         GOTO1 ANY                 ANY DATA?                                    
         CLI   TR2NAMEH+5,0                                                     
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         MVC   OINAME,WORK         COPY INPUT                                   
         GOTO1 ADDELEM             ADD THE ELEM                                 
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,OIGENELQ     SYSTEM ID ELEMENT                            
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING OIGEND,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   OIGENEL,OIGENELQ                                                 
         MVI   OIGENLN,OIGENLNQ                                                 
*                                                                               
         LA    R2,TR2SYSCH                                                      
         GOTO1 ANY                                                              
         CLI   TR2SYSCH+5,0              TWO CHARS EX SY,DC,HR ETC              
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   TR2SYSCH+5,2              TWO CHARS EX SY,DC,HR ETC              
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         TM    TR2SYSCH+4,X'04'          VALID ALBHABETIC                       
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         BAS   RE,GETDPT                                                        
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   OIGENDPT,WORK             PUT IT IN RECORD                       
*                                                                               
         LA    R2,TR2PHONH               CHK PHONE NUMBER EXTN                  
         GOTO1 ANY                                                              
         CLI   TR2PHONH+5,0              MUST BE 4 NUMBERS                      
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   TR2PHONH+5,4              MUST BE 4 NUMBERS                      
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         TM    TR2PHONH+4,X'08'          MUST BE NUMERIC                        
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         CLI   TR2PHON,X'F5'             MUST START WITH 5                      
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   OIGENPHN,WORK                                                    
*                                                                               
         LA    R2,TR2FAXH                                                       
         GOTO1 ANY                                                              
         CLI   TR2FAXH+5,0               MUST BE 4 NUMBERS                      
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         CLI   TR2FAXH+5,4               MUST BE 4 NUMBERS                      
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         TM    TR2FAXH+4,X'08'           MUST BE NUMERIC                        
         BNZ   *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         CLI   TR2FAX,X'F5'              MUST START WITH 5                      
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   OIGENFAX,WORK                                                    
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,OITXTELQ                                                  
         GOTO1 REMELEM                                                          
         LA    R2,TR2COMNH                                                      
         LA    R3,1                SEQUENCE NUMBER FOR COMMENTS                 
VCOMLP   CLI   5(R2),0                                                          
         BE    VRX                 NO COMMENTS, NOT REQUIRED, EXIT              
         LA    R6,ELEM                                                          
         USING OITXTD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   OITXTEL,OITXTELQ                                                 
         ZIC   R1,5(R2)            LENGTH OF WHAT USER TYPED IN                 
         LA    R1,OITXTLNQ(R1)                                                  
         STC   R1,OITXTLN          LENGTH OF COMMENT                            
         STC   R3,OITXTLEN         ELEMENT SEQUENCE NUMBER                      
*        MVC   SCTXTLN,5(R2)       LENGTH OF INPUT                              
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   OITXT(0),8(R2)                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,68(R2)           BUMP TO NEXT COMMENT FIELD                   
         LA    R3,1(R3)            BUMP UP THE SEQUENCE NUMBER                  
         B     VCOMLP              LOOP                                         
*                                                                               
VRX      B     DR                                                               
*************************************************************                   
*                      DISPLAY RECORD                       *                   
*************************************************************                   
DR       L     R6,AIO                                                           
         MVI   ELCODE,OINAMELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                      NAME ELEMENT IS REQUIRED               
         USING OINAMD,R6                                                        
         MVC   TR2NAME,OINAME                                                   
         OI    TR2NAMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,OIGENELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                      SYSTEM ELEMENT IS REQUIRED             
         USING OIGEND,R6                                                        
         MVC   TR2SYSC,OIGENDPT          2 BYTE SYSTEM CODE                     
         OI    TR2SYSCH+6,X'80'                                                 
*                                        DO TABLE LOOK UP HERE                  
         BAS   RE,GETDPT                                                        
         NI    TR2SYSPH+1,X'FB'          DISPLAY SYSTEM NAME                    
         OI    TR2SYSPH+6,X'80'                                                 
         OI    TR2SYSNH+6,X'80'                                                 
*                                                                               
         MVC   TR2PHON,OIGENPHN                                                 
         OI    TR2PHONH+6,X'80'                                                 
*                                                                               
         MVC   TR2FAX,OIGENFAX                                                  
         OI    TR2FAXH+6,X'80'                                                  
*                                                                               
         LA    R3,0                                                             
         L     R6,AIO              R6 NOW POINTS TO THE RECORD                  
         USING OITXTD,R6                                                        
         MVI   ELCODE,OITXTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         LA    R2,TR2COMNH         R2 POINTS TO FIRST COMMENT FIELD             
COMLOOP  AR    R2,R3               BUMP R2 TO ANOTHER LINE OF COMMENTS          
         ZIC   R1,OITXTLN                GET THE LENGTH OF COMMENT              
         SHI   R1,OITXTLNQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),OITXT             DISPLAY COMMENT                        
         OI    6(R2),X'80'               TRANSMIT                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         LA    R3,68               SPACE TO THE NEXT COMMENT                    
         B     COMLOOP                                                          
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
* DRX      CLI   ACTNUM,ACTADD                                                  
         BE    DRARND                                                           
         MVC   KEY,KEY2                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
DRARND   B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*                       DISPLAY KEY                         *                   
*************************************************************                   
DK       L     R4,AIO                                                           
         USING OINFKEYD,R4                                                      
*                                                                               
         MVC   TR2UID,OINFSTAF                                                  
         OI    TR2UIDH+6,X'80'                                                  
         MVC   USERID,OINFSTAF                                                  
         LA    R4,KEY2                                                          
         MVC   OINFSTAF,USERID                                                  
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
                                                                                
*************************************************************                   
*                      LIST RECORDS                         *                   
*************************************************************                   
* LR       LA    R4,KEY                                                         
*        USING SCHOKEYD,R4                                                      
*        OC    KEY,KEY                                                          
*        BNZ   LR10                NOT THE FIRST TIME FOR LIST                  
*                                                                               
*        MVI   SCHOSYS,SCHOSYSQ                                                 
*        MVI   SCHOTYP,SCHOTYPQ                                                 
*        MVC   SCHOSIDC,TR2ID                                                   
*        MVC   SAVEKEY,KEY                                                      
*                                                                               
* LR10     GOTO1 HIGH                                                           
*        CLI   DMCB+8,0                                                         
*        BE    LR30                                                             
*        DC    H'0'                                                             
*                                                                               
* LR20     LA    R4,KEY                                                         
*        GOTO1 SEQ                                                              
*        CLI   DMCB+8,0                                                         
*        BE    LR30                                                             
*        DC    H'0'                                                             
*                                                                               
* LR30     CLC   KEY(2),SAVEKEY      HAS THE KEY CHANGED?                       
*         BNE   LRX                 IF CHANGED, EXIT LIST                       
*                                                                               
*        GOTO1 GETREC              GET THE RECORD                               
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        L     R4,AIO                                                           
*                                                                               
*        MVC   LISTAR,SPACES                                                    
*        MVC   LSTID,SCHOSIDC      PUT ID ON THE LIST.                          
*                                                                               
*        L     R6,AIO                                                           
*        MVI   ELCODE,SCNAMELQ     GET THE NAME ELEMENT                         
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING SCNAMD,R6                                                        
*        MVC   LSTNAME,SCNAME      PUT NAME ON THE LIST.                        
*                                                                               
*        L     R6,AIO                                                           
*        MVI   ELCODE,SCEXTELQ     GET THE EXTENSION ELEMENT                    
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING SCEXTD,R6                                                        
*        MVC   LSTPHON,SCPHONUM    PUT PHONE EXT. ON THE LIST.                  
*                                                                               
*        L     R6,AIO                                                           
*        MVI   ELCODE,SCSYSELQ     GET THE DEPARTMENT ELEMENT                   
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING SCSYSD,R6                                                        
*        MVC   LSTDEPCO,SCSYSIDN   PUT THE DEPARTMENT CODE ON THE LIST.         
*                                                                               
*        MVC   KEY2,KEY                                                         
*        LA    R4,KEY                                                           
*        USING DPTKEYD,R4                                                       
*        XC    KEY,KEY                                                          
*        MVI   DPTKSYS,DPTKSYSQ    BUILD THE KEY FOR DEPT RECS                  
*        MVI   DPTKSTYP,DPTKSTYQ                                                
*        MVC   DPTKCODE,SCSYSIDN                                                
*        MVC   AIO,AIO2            SET OUTPUT TO AIO2                           
*        L     R6,AIO                                                           
*        GOTO1 READ                                                             
*        GOTO1 GETREC                                                           
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                DEPARTMENT RECORD HAS TO BE THERE            
*        MVI   ELCODE,DPTHEDEQ                                                  
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        USING DPTHEDD,R6                                                       
*        MVC   LSTDEPNA,DPTHEDTX                                                
*        MVC   AIO,AIO1            PUT OUTPUT BACK TO DEFAULT                   
*        MVC   KEY,KEY2            RESTORE THE ORIGINAL KEY                     
*        GOTO1 READ                                                             
*        GOTO1 GETREC                                                           
*                                                                               
*        GOTO1 LISTMON                                                          
*        B     LR20                                                             
*                                                                               
* LRX      B     XIT                                                            
*        DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*                    PRINT RECORDS LATER                    *                   
*************************************************************                   
GETDPT   NTR1                                                                   
         USING DPTTABD,R4                                                       
         XC    TR2SYSN,TR2SYSN                                                  
*        LA    R2,TR2SYSCH                                                      
         LA    R4,DPTTAB                                                        
TABLOOP  CLI   DPTCODE,X'FF'                                                    
         BE    NOTINTAB                                                         
         CLC   TR2SYSC,DPTCODE                                                  
         BE    GD                                                               
         LA    R4,DPTTABDQ(R4)                                                  
         B     TABLOOP                                                          
GD       MVC   TR2SYSN,DPTNAME                                                  
         CR    RB,RB                                                            
*        OI    TR2SYSNH+6,X'80'                                                 
         DROP  R4                                                               
         B     *+6                                                              
NOTINTAB CR    RB,R4                                                            
         XIT1                                                                   
         SPACE 5                                                                
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
RELO     DS    F                                                                
SRTCARD  DC    CL80'SORT FIELDS=(1,2,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=2002'                                  
*                                                                               
DPTTAB   DC    CL2'CM',CL25'COMMUNICATIONS'                                     
         DC    CL2'CS',CL25'CLIENT SERVICE'                                     
         DC    CL2'DC',CL25'DATA CONTROL'                                       
         DC    CL2'HR',CL25'HUMAN RESOURCES'                                    
         DC    CL2'OP',CL25'OPERATIONS'                                         
         DC    CL2'SY',CL25'SYSTEM'                                             
         DC    X'FFFF'                                                          
*                                                                               
DPTTABD  DSECT                                                                  
DPTCODE  DS    CL2                                                              
DPTNAME  DS    CL25                                                             
DPTTABDQ EQU   *-DPTTABD                                                        
*                                                                               
         SPACE 5                                                                
         EJECT                                                                  
                                                                                
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMCED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* ++INCLUDE CTSFMBED                                                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
* ++INCLUDE CTSFMAED                                                            
         EJECT                                                                  
* ++INCLUDE CTGENSCHO                                                           
         EJECT                                                                  
       ++INCLUDE CTGENRGUP                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
USERID   DS    0CL8                IDDDCITY                                     
ID       DS    CL4                                                              
DD       DS    CL2                                                              
STCODE   DS    CL2                                                              
KEY2     DS    XL(L'KEY)           BACKUP KEY                                   
SAVEKEY  DS    XL32                GENFILE KEY                                  
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTID    DS    CL5                 OFFICE ID                                    
         DS    CL3                                                              
LSTNAME  DS    CL24                STAFF NAME                                   
         DS    CL3                                                              
LSTPHON  DS    CL4                 PHONE EXTENSION                              
         DS    CL3                                                              
LSTDEPCO DS    CL2                 DEPARTMENT CODE                              
         DS    CL3                                                              
LSTDEPNA DS    CL12                DEPARTMENT NAME                              
         SPACE 5                                                                
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTID    DS    CL5                 OFFICE ID                                    
         DS    CL3                                                              
PRTNAME  DS    CL24                NAME                                         
         DS    CL3                                                              
PRTPHO   DS    CL4                 PHONE EXTENSION                              
         DS    CL3                                                              
PRTFAX   DS    CL4                 FAX EXTENSION                                
         DS    CL3                                                              
PRTSYS   DS    CL12                DEPARTMENT NAME                              
         DS    CL3                                                              
         ORG   P                                                                
         DS    CL5                 TAB THE COMMENTS                             
PRTCOM1  DS    CL60                COMMENT ONE                                  
         DS    C                   RIGHT BOX MARGIN                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142RGUP3E    11/08/99'                                      
         END                                                                    
