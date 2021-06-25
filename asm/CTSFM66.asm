*          DATA SET CTSFM66    AT LEVEL 104 AS OF 02/27/09                      
*PHASE TA0A66A                                                                  
*                                                                               
* *********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM66 -- MQDEF RECORDS MAINTENANCE/LIST            *         
*                                                                     *         
*  COMMENTS:     MAINTAINS MQDEF RECORD ON GENDIR/GENFIL              *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMB6 (MAINTENANCE)                        *         
*                        CTSFMC5 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED MQDEF RECORDS, LIST                          *         
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
         TITLE 'TA0A66 - MQDEF RECORD MAINT/LIST'                               
TA0A66   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A66**,R7,RR=R3                                              
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
         OI    GENSTAT4,NODELLST   DELETE NOT ALLOWED FROM LIST                 
*                                                                               
         CLI   MODE,RECDEL         DELETE NOT ALLOWED                           
         BNE   MODE01                                                           
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     VSFMERR                                                          
*                                                                               
MODE01   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              VALIDATE KEY                                           *         
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
*                                                                               
         LA    R3,KEY              BUILD THE KEY                                
         USING MQDEFD,R3                                                        
*                                                                               
         MVI   MQDKSYS,MQDKSYSQ    X'00'                                        
         MVI   MQDKTYP,MQDKTYPQ    X'82'                                        
*                                                                               
         LA    R2,MQUAPPH                                                       
         CLI   ACTEQU,ACTLIST      KEY FIELDS CAN BE EMPTY ON LIST              
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK05                SKIP APPLICATION VALIDATION                  
         GOTO1 ANY                 APPLICATION IS REQUIRED                      
         MVC   MQDAPPL,WORK        APPLICATION                                  
*                                                                               
VK05     LA    R2,MQUTYPH                                                       
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK06                                                             
         CLI   5(R2),0                                                          
         BNE   VK06                SKIP TYPE VALIDATION                         
         XC    MQLTYPN,MQLTYPN     CLEAR THE OBJECT TYPE NAME FIELD             
         OI    MQLTYPNH+6,X'80'                                                 
         B     VK21                                                             
VK06     GOTO1 ANY                 OBJECT TYPE IS REQUIRED                      
         LA    R5,TYPTAB                                                        
VK10     CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    ERRINV                                                           
         CLC   8(1,R2),0(R5)                                                    
         BE    VK20                                                             
         LA    R5,21(R5)           NEXT ENTRY                                   
         B     VK10                                                             
VK20     MVC   MQUTYPN,1(R5)                                                    
         OI    MQUTYPNH+6,X'80'                                                 
         MVC   MQDOTYP,MQUTYP                                                   
*                                                                               
VK21     LA    R2,MQUIDNH                                                       
         CLI   ACTNUM,ACTADD       ID NUMBER HAS TO BE ASSIGNED ON ADD          
         BNE   VK40                                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(23),KEY     SAME APPLICATION, SAME TYPE?                 
         BE    VK25                                                             
         MVC   KEY,KEYSAVE         RESTORE THE KEY                              
         LH    R4,=X'FFFE'         NEW APPLICATION, NEW TYPE                    
         B     VK30                ID NUMBER START FROM 1                       
*                                                                               
VK25     ZICM  R4,MQDID,2          ID OF PREVIOUS RECORD ADDED                  
         CH    R4,=X'0001'                                                      
         BE    ERRFUL              NO MORE ID SLOTS AVAILABLE                   
         BCTR  R4,R0               THE NEXT AVAILABLE ID NUMBER                 
VK30     STCM  R4,3,MQDID                                                       
         STCM  R4,3,HALF           FOR ARITHMATIC CALCULATIONS                  
         XC    HALF,=X'FFFF'       FLIP THE BITS                                
         EDIT  HALF,MQUIDN,ALIGN=LEFT                                           
         OI    MQUIDNH+6,X'80'     TRANSMIT                                     
         B     VK50                                                             
*                                                                               
VK40     CLI   ACTEQU,ACTLIST                                                   
         BE    VK50                                                             
         GOTO1 ANY                 ID NUMBER IS REQUIRED                        
         TM    4(R2),X'08'         IS INPUT FIELD VALID NUMERIC                 
         BNO   ERRINV                                                           
         CLI   8(R2),C'0'          0 IS AN INVALID ID NUMBER                    
         BE    ERRINV                                                           
         XC    DUB,DUB                                                          
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)         EBCDIC --> BINARY                            
         CVB   R4,DUB                                                           
         STCM  R4,3,HALF                                                        
         XC    HALF,=X'FFFF'       FLIP THE BITS                                
         MVC   MQDID,HALF          ID NUMBER                                    
*                                                                               
*        ID NUMBER IS STORED IN 1'S COMPLEMENT FORM TO GIVE US                  
*        EASY ACCESS TO THE LAST ID ADDED BY DOING A READ HIGH                  
*                                                                               
         DROP  R3                                                               
VK50     MVC   SAVEKEY,KEY                                                      
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VKX                                                              
         XC    KEY,KEY             CLEAR KEY TO SHOW FIRST TIME AT LIST         
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE RECORD: BUILD THE RECORD (3 ELEMENT)          *         
***********************************************************************         
VR       DS    0H                                                               
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         L     R6,AIO                                                           
         USING MQDGEND,R6                                                       
         MVI   ELCODE,MQDGENCQ     SEARCH FOR X'10' ELEMENT                     
         BAS   RE,GETEL            IF ELEMENT ALREADY EXIST, CHANGE IT          
         BE    VR10                                                             
*                                  ELSE, ADD IT                                 
         LA    R6,ELEM             BUILD THE GENERAL INFO ELEMENT               
         XC    ELEM,ELEM                                                        
VR10     LA    R2,MQUOWNH          OWNER IS REQUIRED                            
         GOTO1 ANY                                                              
         MVI   MQDGENC,MQDGENCQ    ELCODE X'10'                                 
         MVC   MQDGENOW,WORK                                                    
         MVI   MQDGENLN,MQDGENLQ                                                
*                                                                               
         LA    R2,MQUOVNH                                                       
         CLI   MQUOVNH+5,0         IS OUTPUT VERSION NUMBER EMPTY?              
         BE    VR12                YES - SKIP IT                                
         CLI   MQUOVN,C'1'         1 - 9                                        
         BL    ERRINV                                                           
         CLI   MQUOVN,C'9'                                                      
         BH    ERRINV                                                           
         MVC   MQDGENOV,MQUOVN                                                  
*                                                                               
VR12     LA    R2,MQUTSTH                                                       
         CLI   MQUTSTH+5,0         IS TEST ONLY FIELD EMPTY?                    
         BE    VR12X               YES - SKIP IT                                
         NI    MQDGENFL,X'FF'-MQDGFTQ                                           
         CLI   MQUTST,C'N'                                                      
         BE    VR12X                                                            
         CLI   MQUTST,C'Y'                                                      
         BNE   ERRINV                                                           
         OI    MQDGENFL,MQDGFTQ                                                 
VR12X    EQU   *                                                                
*                                                                               
VR14     LA    R2,MQUBCH                                                        
         CLI   MQUBCH+5,0          IS BC FIELD EMPTY?                           
         BE    VR14X               YES - SKIP IT                                
         NI    MQDGENFL,X'FF'-MQDGFBCQ                                          
         CLI   MQUBC,C'N'                                                       
         BE    VR14X                                                            
         CLI   MQUBC,C'Y'                                                       
         BNE   ERRINV                                                           
         OI    MQDGENFL,MQDGFBCQ                                                
VR14X    EQU   *                                                                
*                                                                               
VR30     CLI   ACTEQU,ACTADD                                                    
         BNE   VR40                                                             
         GOTO1 ADDELEM             ADD THE X'10' ELEMENT                        
*                                                                               
VR40     L     R6,AIO                                                           
         USING MQDNAMD,R6                                                       
         MVI   ELCODE,MQDNAMCQ                                                  
         BAS   RE,GETEL                                                         
         BE    VR45                                                             
*                                                                               
         LA    R6,ELEM             BUILD THE OBJECT NAME ELEMENT                
         XC    ELEM,ELEM                                                        
VR45     LA    R2,MQUNAMEH                                                      
         GOTO1 ANY                 OBJECT NAME IS REQUIRED                      
         LA    R5,48               GET THE LENGTH OF THE FIELD                  
         LA    R4,MQUNAME+47       VALIDATE BACKWARDS                           
VR50     CLI   0(R4),X'40'         IS IT A SPACE?                               
         BE    VR55                                                             
         CLI   0(R4),0                                                          
         BNE   VR60                NO MORE TRAILING SPACE/NULLS                 
VR55     BCTR  R4,R0                                                            
         BCT   R5,VR50                                                          
*                                                                               
VR60     CLI   0(R4),C'.'          '.'                                          
         BE    VR70                                                             
         CLI   0(R4),C'/'          '/'                                          
         BE    VR70                                                             
         CLI   0(R4),C'_'          '_'                                          
         BE    VR70                                                             
         CLI   0(R4),C'%'          '%'                                          
         BE    VR70                                                             
         CLI   0(R4),C'A'          A - Z                                        
         BL    ERRINV                                                           
         CLI   0(R4),C'Z'                                                       
         BNH   VR70                                                             
         CLI   0(R4),C'0'          0 - 9                                        
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BNH   VR70                                                             
         B     ERRINV              ALL OTHERS ARE INVALID CHARS                 
VR70     BCTR  R4,R0               NEXT CHAR                                    
         BCT   R5,VR60             LOOP USING LENGTH AS COUNTER                 
         MVI   MQDNAMC,MQDNAMCQ    ELCODE X'20'                                 
         MVC   MQDNAME,WORK        BLANK PADDED WITH SPACE                      
         MVI   MQDNAMLN,MQDNAMLQ                                                
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR80                                                             
         GOTO1 ADDELEM             ADD THE X'20' ELEMENT                        
*                                                                               
VR80     L     R6,AIO                                                           
         USING MQDDESD,R6                                                       
         MVI   ELCODE,MQDDESCQ                                                  
         BAS   RE,GETEL                                                         
         BE    VR90                                                             
*                                                                               
         LA    R6,ELEM             BUILD THE DESCRIPTION ELEMENT                
         XC    ELEM,ELEM                                                        
VR90     CLI   MQUDESCH+5,0        IS THE DESCRIPTION FIELD EMPTY?              
         BE    VRX                                                              
         LA    R2,MQUDESCH                                                      
         GOTO1 ANY                 BLANK PAD WITH SPACES                        
         MVI   MQDDESC,MQDDESCQ                                                 
         MVC   MQDDESCR,WORK                                                    
         MVI   MQDDESLN,MQDDESLQ                                                
         CLI   ACTEQU,ACTADD                                                    
         BNE   VRX                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     DR                                                               
         EJECT                                                                  
***********************************************************************         
*              DISPLAY RECORD                                         *         
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         USING MQDGEND,R6                                                       
*                                                                               
         MVI   ELCODE,X'10'        GET THE GENERAL INFO ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   ERRREC              MUST BE AN ERROR IN THE RECORD               
         MVC   MQUOWN,MQDGENOW                                                  
         OI    MQUOWNH+6,X'80'                                                  
*                                                                               
         MVC   MQUOVN,MQDGENOV                                                  
*                                                                               
         MVI   MQUBC,C'N'                                                       
         TM    MQDGENFL,MQDGFBCQ                                                
         BNO   *+8                                                              
         MVI   MQUBC,C'Y'                                                       
*                                                                               
         MVI   MQUTST,C'N'                                                      
         TM    MQDGENFL,MQDGFTQ                                                 
         BNO   *+8                                                              
         MVI   MQUTST,C'Y'                                                      
*                                                                               
         L     R6,AIO                                                           
         USING MQDNAMD,R6                                                       
*                                                                               
         MVI   ELCODE,X'20'        GET THE OBJECT NAME ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   ERRREC              OBJECT NAME IS ALSO REQUIRED                 
         MVC   MQUNAME,MQDNAME                                                  
         OI    MQUNAMEH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         USING MQDDESD,R6                                                       
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                 DESCRIPTION IS NOT MENDATORY                 
         MVC   MQUDESC,MQDDESCR                                                 
         OI    MQUDESCH+6,X'80'                                                 
                                                                                
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY KEY                                            *         
***********************************************************************         
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING MQDEFD,R6                                                        
*                                                                               
         MVC   MQUAPP,MQDAPPL                                                   
         OI    MQUAPPH+6,X'80'                                                  
*                                                                               
         MVC   MQUTYP,MQDOTYP                                                   
         OI    MQUTYPH+6,X'80'                                                  
*                                                                               
         LA    R5,TYPTAB                                                        
DK10     CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    ERRREC                                                           
         CLC   MQDOTYP,0(R5)                                                    
         BE    DK20                                                             
         LA    R5,21(R5)           NEXT ENTRY                                   
         B     DK10                                                             
DK20     MVC   MQUTYPN,1(R5)                                                    
         OI    MQUTYPNH+6,X'80'                                                 
         MVC   HALF,MQDID                                                       
         XC    HALF,=X'FFFF'                                                    
         EDIT  HALF,MQUIDN,ALIGN=LEFT                                           
         OI    MQUIDNH+6,X'80'                                                  
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              ON-SCREEN LIST                                         *         
***********************************************************************         
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING MQDEFD,R6                                                        
         OC    KEY,KEY             FIRST TIME IN LIST?                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,SAVEKEY                                                      
LR10     GOTO1 HIGH                                                             
         B     LR25                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
*                                                                               
LR25     CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     DS    0H                                                               
         LA    R6,KEY                                                           
         CLC   KEY(2),SAVEKEY      ARE WE STILL ON MQDEF RECORDS?               
         BNE   LRX                                                              
         CLI   MQLTYPH+5,0                                                      
         BE    LR40                                                             
         CLC   MQDOTYP,MQLTYP                                                   
         BNE   LR20                                                             
         DROP  R6                                                               
*                                                                               
LR40     GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         L     R6,AIO                                                           
         USING MQDEFD,R6                                                        
*                                                                               
         MVC   HALF,MQDID                                                       
         XC    HALF,=X'FFFF'                                                    
         EDIT  HALF,LSTIDN,ALIGN=LEFT                                           
*                                                                               
         MVC   LSTAPP,MQDAPPL                                                   
*                                                                               
         MVC   LSTTYPE,MQDOTYP                                                  
*                                                                               
         L     R6,AIO                                                           
         USING MQDGEND,R6                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERRREC                                                           
         MVC   LSTOWN,MQDGENOW                                                  
         MVC   LSTOVN,MQDGENOV                                                  
         CLI   MQLOVNH+5,0                                                      
         BE    *+14                                                             
         CLC   LSTOVN,MQLOVN                                                    
         BNE   LR20                                                             
*                                                                               
         MVI   LSTBC,C'N'                                                       
         TM    MQDGENFL,MQDGFBCQ                                                
         BNO   *+8                                                              
         MVI   LSTBC,C'Y'                                                       
         CLI   MQLBCH+5,0                                                       
         BE    *+14                                                             
         CLC   LSTBC,MQLBC                                                      
         BNE   LR20                                                             
*                                                                               
         MVI   LSTTST,C'N'                                                      
         TM    MQDGENFL,MQDGFTQ                                                 
         BNO   *+8                                                              
         MVI   LSTTST,C'Y'                                                      
         CLI   MQLTSTH+5,0                                                      
         BE    *+14                                                             
         CLC   LSTTST,MQLTST                                                    
         BNE   LR20                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING MQDNAMD,R6                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERRREC                                                           
         MVC   LSTNAME,MQDNAME                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
ERRREC   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRFUL   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
*                                                                               
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
***********************************************************************         
*              TABLES                                                 *         
***********************************************************************         
TYPTAB   DC    CL1'M',CL20'QUEUE MANAGER'                                       
         DC    CL1'Q',CL20'QUEUE'                                               
         DC    CL1'N',CL20'NAMELIST'                                            
         DC    CL1'P',CL20'PROCESS DEFINITION'                                  
         DC    CL1'C',CL20'CHANNEL'                                             
         DC    X'FF'                                                            
         SPACE 3                                                                
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         EJECT                                                                  
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMB6D                                                                      
* CTSFMC6D                                                                      
* CTGENMQDEF                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC5D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENMQDEF                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
SAVEKEY  DS    XL32                BACKUP KEY                                   
*                                                                               
         EJECT                                                                  
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTAPP   DS    CL8                 APPLICATION                                  
         DS    CL2                                                              
LSTTYPE  DS    CL1                 OBJECT TYPE                                  
         DS    CL2                                                              
LSTOVN   DS    CL1                 OUTPUT VERSION NUMBER                        
         DS    CL2                                                              
LSTTST   DS    CL1                 TEST ONLY                                    
         DS    CL2                                                              
LSTBC    DS    CL1                 BUSSINESS CONTINUE TEST ALLOW                
         DS    CL2                                                              
LSTIDN   DS    CL3                 UNIQUE ID NUMBER                             
         DS    CL2                                                              
LSTOWN   DS    CL3                 OWNER                                        
         DS    CL2                                                              
LSTNAME  DS    CL30                OBJECT NAME                                  
*                                                                               
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104CTSFM66   02/27/09'                                      
         END                                                                    
