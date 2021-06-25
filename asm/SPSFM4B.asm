*          DATA SET SPSFM4B    AT LEVEL 054 AS OF 10/07/08                      
*PHASE T2174BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174B  -- SUPERVISOR RECORD MAINTENANCE             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SUPERVISOR RECORDS ON SPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMF7, AND SPSFME7                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED SUPERVISOR RECORDS                           *         
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
         TITLE 'T2174B - SUPERVISOR RECORD MAINTENANCE'                         
T2174B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174B**,R7,RR=R3                                              
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
         BAS   RE,SETUP                 SETUP PF-KEY TABLE, ETC.                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECPUT         WRITE SUPV REC BACK TO SPTFIL?               
         BE    RP                  YES, MAKE SURE WE HAVE RIGHT D/A             
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,XRECADD                                                     
         BE    XR                  ADDING & DELETING PASSIVE KEYS               
         CLI   MODE,XRECPUT                                                     
         BE    XR                  ADDING & DELETING PASSIVE KEYS               
         CLI   MODE,RECDEL                                                      
         BE    RCDL                SPECIAL CASES FOR DELETION                   
         CLI   MODE,XRECDEL                                                     
         BE    XR                  PASSIVE KEYS ON DELETE                       
         CLI   MODE,XRECREST                                                    
         BE    XR                  PASSIVE KEYS ON RESTORE                      
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   XYES                NO                                           
         CLC   =C'NOW',CONWHEN     NOW REPORT?                                  
         BE    XYES                YES - NOT SUPPORTED!                         
         BRAS  RE,PR               NO - PRINT REPORT                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* RECPUT                                                                        
*                                                                               
RP       DS    0H                                                               
         L     R6,AIO1                                                          
         CLC   0(2,R6),=X'0D61'         SUPERVISOR RECMUST BE IN AIO1           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'SPVKEY),0(R6)                                              
         MVC   AIO,AIO2                 DON'T BLOW AWAY AIO1                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                   GET D/A OF REC FOR PUTREC               
         MVC   AIO,AIO1                 RESTORE CORRECT AIO FOR PUTREC          
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         CLI   ACTEQU,ACTREP            ACTION REPORT?                          
         BNE   VK09                     NO                                      
         BRAS  RE,VKRPT                                                         
         B     VKXX                                                             
*                                                                               
VK09     MVC   AIO,AIO1                 MAKE SURE USING CORRECT AIO             
         MVC   OFFICE,SPACES            CLEAR STORAGE                           
         MVC   SUPV,SPACES                                                      
         XC    SAVESEL,SAVESEL                                                  
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SECURITY ALPHA                           
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
*                                                                               
         CLI   ACTEQU,ACTLIST           ACTION LIST?                            
         BNE   VK50                     NO, SKIP AHEAD                          
*                                                                               
         LA    R2,SPLOFFH                                                       
         CLI   5(R2),0             DATA IN OFFICE FIELD?                        
         BE    VK10                NO SKIP                                      
         MVC   OFFICE,SPLOFF            STORE OFFICE INPUT                      
         OC    OFFICE,SPACES            SPACE PAD                               
*                                                                               
VK10     LA    R2,SPLSUPH                                                       
         CLI   5(R2),0                  DATA IN SUPERVISOR FIELD?               
         BE    VK100                                                            
         MVC   SUPV,SPLSUP                                                      
         OC    SUPV,SPACES                                                      
         B     VK100                    BUILD KEY FOR DMREAD                    
*                                                                               
VK50     DS    0H                       VALIDATE FOR DISPLAY/ADD/CHANGE         
         LA    R2,SPCOFFH               VALIDATE OFFICE CODE                    
         CLI   5(R2),0                  OFFICE INPUT?                           
         BE    ERRMIS                   NO, MISSING INPUT ERROR                 
         CLI   8(R2),C' '          FIRST CHAR CANNOT BE BLANK OR LESS           
         BNH   ERRINV                                                           
         MVC   OFFICE,SPCOFF                                                    
         OC    OFFICE,SPACES                                                    
         BAS   RE,VALOFF                ROUTINE TO FIND OFFICE REC              
*                                                                               
         LA    R2,SPCSUPH               VALIDATE SUPERVISOR CODE                
         CLI   5(R2),0                  SUPV INPUT?                             
         BE    ERRMIS                   NO, FIELD IS REQUIRED                   
         CLI   8(R2),C' '          FIRST CHAR CANNOT BE BLANK OR LESS           
         BNH   ERRINV                                                           
         CLI   5(R2),2                  SUPV CODE LEN. AT LEAST 2               
         BL    ERRINV                                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK59                                                             
         LA    R1,SPCSUP           VALID ALPHA NUMERIC                          
         LA    R0,L'SPCSUP                                                      
VK54     CLI   0(R1),C' '                                                       
         BNH   VK56                                                             
         CLI   0(R1),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
VK56     LA    R1,1(R1)                                                         
         BCT   R0,VK54                                                          
*                                                                               
VK59     MVC   SUPV,SPCSUP                                                      
         OC    SUPV,SPACES                                                      
*                                                                               
VK100    XC    TEMPFLD,TEMPFLD          BUILD FAKE FIELD FOR VALIMED            
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                  TO GET AGENCY CODE                      
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'               DON'T WANT/NEED MEDIA NIBBLE            
*                                                                               
         LA    R4,KEY              BUILD SUPERVISOR KEY                         
         USING SPVRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE                                                   
         MVC   SPVKSPV,SUPV                                                     
*                                                                               
         CLC   SAVEKEY,KEY              SAME AS LAST INPUT KEY?                 
         BE    *+8                                                              
         OI    STATUS,KEYCHG            DIFF REC, CHANGE STATUS FLAG            
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                 MAKE SURE USING CORRECT AIO             
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   XIT                                                              
*                                                                               
         MVI   FILTFLAG,0                                                       
         XC    FILTCLT,FILTCLT                                                  
         LA    R2,SPLFILH                                                       
         CLI   5(R2),0                  INPUT IN FILTER FIELD?                  
         BE    XIT                      IF NO - EXIT                            
         XC    SCANAREA,SCANAREA                                                
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         CLI   DMCB+4,FILTMAXQ         NUMBER OF FILTERS ,CURRENT MAX=2         
         BH    ERRINV                                                           
*                                                                               
         LA    R6,SCANAREA              FOR NOW, ONLY CLT IS ACCEPTED           
*                                                                               
VKX10    CLI   0(R6),3                  FILTER NAME IS 3 CHARS?                 
         BE    VKXCLT                   YES, TEST FOR CLIENT FILTER             
         CLI   0(R6),4                  FILTER NAME IS 4 CHARS?                 
         BNE   ERRINV                   NO, ERROR                               
*                                                                               
         CLC   =C'PLAN',12(R6)          FILTER MUST START WITH PLAN             
         BNE   ERRINV                                                           
         CLI   1(R6),1              >   RIGHT SIDE OF FILTER EXPRESSION         
         BNE   ERRINV               >   1                                       
         CLI   22(R6),C'Y'                                                      
         BNE   VKX15                                                            
         TM    FILTFLAG,FILTPNYQ+FILTPNNQ                                       
         BNZ   ERRINV                                                           
         OI    FILTFLAG,FILTPNYQ    PLAN=YES                                    
         B     VKX20                                                            
*                                                                               
VKX15    CLI   22(R6),C'N'                                                      
         BNE   ERRINV                                                           
         TM    FILTFLAG,FILTPNYQ+FILTPNNQ                                       
         BNZ   ERRINV                                                           
         OI    FILTFLAG,FILTPNNQ    PLAN=NO                                     
         B     VKX20                                                            
*                                                                               
VKXCLT   CLC   =C'CLT',12(R6)           FILTER MUST START WITH CLT              
         BNE   ERRINV                                                           
         CLI   1(R6),2              >   RIGHT SIDE OF FILTER EXPRESSION         
         BL    ERRINV               >   TWO                                     
         CLI   1(R6),3              >   OR                                      
         BH    ERRINV               >   THREE CHARACTERS ONLY                   
*                                                                               
*        LA    R2,22(R6)             A(CLIENT NAME IN FILTER FIELD)             
*        GOTO1 VALICLT                                                          
         MVC   FILTCLT,22(R6)                                                   
         OI    FILTFLAG,FILTCLTQ                                                
*                                                                               
VKX20    LA    R6,32(R6)                                                        
         CLI   0(R6),0                                                          
         BNE   VKX10                                                            
*                                                                               
VKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
*                                                                               
         BAS   RE,RDPROF                                                        
*                                                                               
         XC    ADDCLTS(ADCLLNEQ),ADDCLTS                                        
         XC    DELCLTS(DECLLNEQ),DELCLTS                                        
         XC    ADDCGRP(ADCGLNEQ),ADDCGRP                                        
         XC    DELCGRP(DECGLNEQ),DELCGRP                                        
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR10                                                             
         MVI   ELCODE,X'01'        SUPERVISOR NAME ELEMENT                      
         GOTO1 REMELEM                  REMOVE ON ACTION CHANGE                 
*                                                                               
VR10     LA    R5,ELEM                                                          
         USING SPVNAMED,R5                                                      
         XC    ELEM,ELEM                                                        
         MVI   SPVNAMEL,X'01'      ELEMENT CODE                                 
         MVI   SPVNAMLN,26         ELEMENT LENGTH                               
*                                                                               
         LA    R2,SPCFNMH          CHECK SUPERVISOR NAME FIELD                  
         CLI   5(R2),0                  INPUT?                                  
         BE    ERRMIS                   NO, ERROR                               
         MVC   SPVFNAME,SPCFNM                                                  
         OC    SPVFNAME,SPACES                                                  
*                                                                               
         LA    R2,SPCLNMH          CHECK SUPERVISOR NAME FIELD                  
         CLI   5(R2),0                  INPUT?                                  
         BE    ERRMIS                   NO, ERROR                               
         MVC   SPVLNAME,SPCLNM                                                  
         OC    SPVLNAME,SPACES                                                  
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
**********************************************************************          
*        VALIDATE PERSONAL ID AND OPTIONS                            *          
*                                                                    *          
         LA    R2,SPCPIDH                                                       
         CLI   5(R2),0             REQ'D                                        
         BE    ERRMIS                                                           
         MVC   PIDNAME,SPCPID                                                   
         OC    PIDNAME,SPACES                                                   
         BAS   RE,VALPID                                                        
         BNE   ERRINV                                                           
*                                                                               
         BAS   RE,VALOPTS                  VALIDATE OPTIONS                     
*                                                                               
         MVI   ELCODE,SPIDELQ      X'22'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING SPIDELD,R5                                                       
         LA    R5,ELEM                                                          
         MVI   SPIDEL,SPIDELQ      X'22'                                        
         MVI   SPIDLN,SPIDLNQ                                                   
         MVC   SPIDNO,PIDNUM                                                    
         MVC   SPIDFLAG,OPTFLAG                                                 
         MVC   SPIDDAYS,DAYS                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,SPCMEDH               FINDING INPUT MEDIA AND ELCODE          
         CLI   5(R2),0                  INPUT MEDIA?                            
         BE    VR35                     NO, SKIP                                
*                                                                               
         LA    R4,MEDTAB                POINT TO BEG OF MEDIA TABLE             
*                                                                               
VR20     CLC   SPCMED,0(R4)             TABLE ENTRY = INPUT?                    
         BE    VR30                     YES, DONE                               
         LA    R4,MTABLQ(R4)            NO, POINT TO NEXT ENTRY                 
         CLI   0(R4),X'FF'              END OF TABLE?                           
         BNE   VR20                     NO, COMPARE TO INPUT AGAIN              
         B     ERRINV                   YES, INVALID INPUT MEDIA                
*                                                                               
VR30     MVC   MEDELCD,1(R4)            STORE ELCODE FROM TABLE                 
         MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1                 MUST RESTORE PROPER IO AREA             
         B     VR40                     CHECK ADD CLIENT FIELDS                 
*                                                                               
* CHECK ADD CLIENT FIELDS W/ NO MEDIA INPUT                                     
VR35     LA    R2,SPCFCLH                                                       
*                                                                               
         CLI   5(R2),0                  NO MEDIA INPUT, CLIENT INPUT?           
         BE    VRX                      NO, ADD THE RECORD                      
         LA    R2,SPCMEDH               YES, REPORT MISSING MEDIA               
         B     ERRMIS                                                           
*                                                                               
**** CHECK ADD CLIENT FIELDS                                                    
*                                                                               
VR40     LA    R2,SPCFCLH               POINT TO FIRST ADD CLIENT FIELD         
         CLI   5(R2),0                                                          
         BE    VRX                                                              
*                                                                               
         BAS   RE,ISVALCGR              1ST ENTRY VALID CLIENT GROUP?           
         BE    VRCG00                   YES                                     
*                                                                               
         LA    R3,ADDCLTS               STORAGE TABLE OF ADDED CLIENTS          
         LA    R5,DELCLTS               STORAGE TABLE OF DEL'D CLIENTS          
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD           CLIENT ELEMENT (BASED ON MEDIA)         
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                 ANY CLIENT ELEMENTS?                    
         BE    ERRMIX2                  YES, CANT MIX CLT & CLTGRPS             
*                                                                               
VR45     CLI   5(R2),0                  MEDIA INPUT, CLIENT INPUT?              
         BE    VRX                      NO CLIENT, JUST ADD REC                 
*                                                                               
         CLI   ACTEQU,ACTADD            ACTION ADD?                             
         BE    VR50                     YES, GOTO ADD CLIENT LOGIC              
*                                                                               
         CLI   8(R2),C'-'               MINUS SIGN ON ACTION CHANGE?            
         BNE   VR50                     NO, GOTO ADD CLIENT LOGIC               
         MVC   CLTCODE,9(R2)            DON'T MOVE IN MINUS                     
         CLI   CLTCODE+2,C'/'           FILTER NOT NEEDED ON DELCLT             
         BNE   *+8                                                              
         MVI   CLTCODE+2,X'40'          REPLACE '/' WITH SPACE                  
         OC    CLTCODE,SPACES                                                   
*                                                                               
         LA    R1,ADDCLTS               MINUS SIGN, SUBT. CLT LOGIC             
         LA    R0,ADCLEND                                                       
VR47     DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   CLTCODE,0(R1)            WON'T LET YOU DELETE A CLIENT           
         BE    ERRDUP                   IF YOU ADDED IT IN A PREVIOUS           
         LA    R1,L'ADDCLTS(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VR47                                                             
*                                                                               
         MVC   0(L'CLTCODE,R5),CLTCODE  ENTRY OK, STORE IN DEL TABLE            
         BAS   RE,DELCLI                                                        
         LA    R5,L'DELCLTS(R5)         PT. TO NEXT AVAIL. DEL STORAGE          
         B     VR55                                                             
*                                                                               
VR50     MVC   CLTCODE,8(R2)            DON'T MOVE IN MINUS                     
         CLI   CLTCODE+2,C'/'                                                   
         BNE   *+8                                                              
         MVI   CLTCODE+2,X'40'                                                  
         OC    CLTCODE,SPACES                                                   
*                                                                               
         LA    R1,DELCLTS               ADD MKT LOGIC                           
         LA    R0,DECLEND                                                       
VR52     DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   CLTCODE,0(R1)            WON'T LET YOU ADD A CLIENT              
         BE    ERRDUP                   IF YOU DELETED IT IN A PREVIOUS         
         LA    R1,L'DELCLTS(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VR52                                                             
*                                                                               
         MVC   0(L'CLTCODE,R3),CLTCODE  ENTRY OK, STORE IN ADD TABLE            
         BAS   RE,ADDCLI                                                        
         LA    R3,L'ADDCLTS(R3)         PT. TO NEXT AVAIL. ADD STORAGE          
*                                                                               
VR55     BAS   RE,NXTSCRF               POINT TO NEXT ADD CLT FIELD             
         LA    R0,SPCLCLH                                                       
         CR    R2,R0                    PAST LAST ADD CLIENT FIELD?             
         BNH   VR45                     NO, CHECK NEXT FIELD                    
         B     VRX                                                              
*                                                                               
VRCG00   LA    R3,ADDCGRP               STORAGE TABLE OF ADDED CGRPS            
         LA    R5,DELCGRP               STORAGE TABLE OF DEL'D CGRPS            
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD           CLIENT ELEMENT (BASED ON MEDIA)         
         BAS   RE,GETEL                 ANY CLIENT ELEMENTS?                    
         BE    ERRMIX                   YES, CANT MIX CLT & CLTGRPS             
*                                                                               
VRCG10   CLI   5(R2),0                                                          
         BE    VRX                                                              
*                                                                               
         BAS   RE,ISVALCGR              IS THIS A VALID CLIENT GROUP?           
         BNE   ERRCGRP                  NO, ERROR - CANT MIX CLT/CLTGRP         
*                                  CGROUP SET ON EXIT                           
*                                                                               
         CLI   8(R2),C'-'               DELETE CLIENT GROUP?                    
         BNE   VRCG30                   NO, ADD IT                              
*                                                                               
         LA    R1,ADDCGRP               DEL CLIENT GROUP LOGIC                  
         LA    R0,ADCGEND                                                       
VRCG20   DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   CGROUP,0(R1)             WON'T LET YOU DELETE A CLIENT           
         BE    ERRDUPCG                 IF YOU ADDED IT IN A PREVIOUS           
         LA    R1,L'ADDCGRP(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VRCG20                                                           
*                                                                               
         LA    R1,9(R2)                                                         
         BAS   RE,DELCLGRP              DELETE CLTGRP FROM SUPV REC             
         MVC   0(3,R5),CGROUP           ENTRY OK, STORE IN DEL TABLE            
         LA    R5,L'DELCGRP(R5)         PT. TO NEXT AVAIL. DEL STORAGE          
         B     VRCG50                                                           
*                                                                               
VRCG30   LA    R1,DELCGRP               ADD CLIENT GROUP LOGIC                  
         LA    R0,DECGEND                                                       
VRCG40   DS    0H                       CHECKS FOR DUP CLIENT ENTRIES           
         CLC   CGROUP,0(R1)             WON'T LET YOU ADD A CLIENT              
         BE    ERRDUPCG                 IF YOU DELETED IT IN A PREVIOUS         
         LA    R1,L'DELCGRP(R1)         "ADD CLIENT" FIELD                      
         CR    R1,R0                                                            
         BNE   VRCG40                                                           
*                                                                               
         LA    R1,8(R2)                 POINT TO C'-' OR CLTGRP                 
         BAS   RE,ADDCLGRP              ADD CGRP TO SUPV REC                    
         MVC   0(3,R3),CGROUP           ENTRY OK, STORE IN ADD TABLE            
         LA    R3,L'ADDCLTS(R3)         PT. TO NEXT AVAIL. ADD STORAGE          
*                                                                               
VRCG50   BAS   RE,NXTSCRF               POINT TO NEXT ADD CLT FIELD             
         LA    R0,SPCLCLH                                                       
         CR    R2,R0                    PAST LAST ADD CLIENT FIELD?             
         BNH   VRCG10                   NO, CHECK NEXT FIELD                    
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL   STAY HERE FOR ONE TRANSACTION                
         B     DR                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        SUPERVISOR NAME ELEMENT                      
         BAS   RE,GETEL                                                         
*                                                                               
         USING SPVNAMED,R6                                                      
         MVC   SPCFNM,SPVFNAME      DISPLAY SUPERVISOR NAME                     
         OI    SPCFNMH+6,X'80'                                                  
         MVC   SPCLNM,SPVLNAME      DISPLAY SUPERVISOR NAME                     
         OI    SPCLNMH+6,X'80'                                                  
*                                                                               
*        DISPLAY PID NAME AND OPTIONS                                           
*                                                                               
         MVC   SPCOPT,SPACES       CLEAR OPTIONS AND PID                        
         OI    SPCOPTH+6,X'80'                                                  
         MVC   SPCPID,SPACES                                                    
         OI    SPCPIDH+6,X'80'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'             PID                                     
         BAS   RE,GETEL                                                         
         BNE   DR3                                                              
         USING SPIDELD,R6                                                       
         MVC   PIDNUM,SPIDNO                                                    
         BAS   RE,GETPIDNM                                                      
         MVC   SPCPID,PIDNAME                                                   
*                                                                               
         LA    R2,SPCOPT                                                        
         TM    SPIDFLAG,SPIDFPLN   IS SUPV A PLANNER                            
         BNO   DR2                 NOPE                                         
         MVC   0(6,R2),=C'PLAN=Y'                                               
         LA    R2,6(R2)                                                         
         MVI   0(R2),C','                                                       
*                                                                               
DR2      CLI   SPIDDAYS,0          WANT SPECIFIC LENGTH?                        
         BNE   DR2A                YES                                          
         MVI   0(R2),0                                                          
         B     DR3                                                              
*                                                                               
DR2A     CLI   0(R2),C','          HAVE OPT DISPLAYED BEFORE?                   
         BNE   *+8                 NO                                           
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'D='                                                   
         EDIT  SPIDDAYS,(2,2(R2)),ALIGN=LEFT                                    
*                                                                               
DR3      BAS   RE,CLTPSC                CLEAR TOP PORTION OF SCREEN             
*                                                                               
**** "LIST BY" LOGIC                                                            
         LA    R2,SPCLISH                                                       
*                                                                               
         CLI   ACTEQU,ACTSEL            LIST?                                   
         BE    DR5                      YES, DEFAULT TO CLT LIST                
         CLI   5(R2),0                  "LIST BY" INPUT AT ALL?                 
         BNE   DR10                     YES, CONTINUE                           
DR5      MVI   8(R2),C'C'               NO, DEFAULT TO CLIENT LIST              
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'80'                                                      
*                                                                               
* LIST BY CLIENT                                                                
*                                                                               
DR10     DS    0H                                                               
         CLI   8(R2),C'C'               "LIST BY" INPUT = C?                    
         BNE   DR50                     NO, CHECK FOR OTHER INPUT               
         TM    4(R2),X'80'              "LIST BY" INPUT THIS TIME?              
         BNO   DR15                     NO, SAME AS BEFORE                      
         XC    STARTCLT,STARTCLT        YES DISPLAY FROM 1ST CLIENT             
         XC    FRSTCLT,FRSTCLT                                                  
         XC    LASTCLT,LASTCLT                                                  
*                                                                               
DR15     MVC   SPCSTRT,CLHEAD           SHOW CLIENT HEADER                      
         OI    SPCSTRTH+6,X'80'                                                 
         BAS   RE,NOSELECT                                                      
         BRAS  RE,CLBTSC                                                        
*                                                                               
         LA    R2,SPCMEDH                                                       
         CLI   ACTEQU,ACTCHA            ACTION CHANGE?                          
         BE    *+12                     YES, DISPLAY FROM FIRST CLIENT          
         TM    4(R2),X'80'              MEDIA INPUT THIS TIME?                  
         BNO   DR22                     NO, DISPLAY FROM LAST POS               
         XC    STARTCLT,STARTCLT        YES, DISPLAY FROM FIRST CLIENT          
         XC    FRSTCLT,FRSTCLT                                                  
         XC    LASTCLT,LASTCLT                                                  
*                                                                               
DR22     CLI   5(R2),0                  MEDIA INPUT?                            
         BNE   DR25                     YES, CONTINUE                           
         MVI   8(R2),C'T'               NO, DEFAULT TO TELEVISION               
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR25     LA    R4,MEDTAB                                                        
*                                                                               
DR30     CLC   SPCMED,0(R4)                                                     
         BE    DR35                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR30                                                             
         B     ERRINV                                                           
*                                                                               
DR35     MVC   MEDELCD,1(R4)            ELCODE FOUND                            
*                                                                               
**** MAKE VALIMED CALL SO CAN MAKE VALICLT CALL ****                            
         MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
* RESTORE THE RECORD!! AIO = AIO2                                               
         MVC   AIO,AIO1                                                         
*                                                                               
* PFKEY STUFF                                                                   
         CLI   PFKEY,8                  DOWN?                                   
         BNE   DR36                     NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTCLT,FRSTCLT          YES,DON'T PAGE DOWN                     
         MVC   STARTCLT,LASTCLT                                                 
         B     DR37                                                             
*                                                                               
DR36     CLI   PFKEY,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTCLT,FRSTCLT          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTCLT,FRSTCLT         IF ANY OTHER PFKEY,START                
*                                                                               
DR37     XC    FRSTCLT,FRSTCLT                                                  
         XC    LASTCLT,LASTCLT                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTCLT,STARTCLT        YES, DISPLAY FROM FIRST CLIENT          
***                                                                             
* CHECK AND DISPLAY (IF EXISTS) CLIENT GROUPS, ELSE DISPLAY CLIENTS             
***                                                                             
         USING LISD,R3                                                          
         LA    R3,SPCSELH               POINT TO 1ST SELECT FIELD               
*                                                                               
         L     R6,AIO                                                           
         USING SPVCGRD,R6                                                       
         MVI   ELCODE,X'12'                                                     
         CLI   SPCMED,C'R'                                                      
         BNE   *+12                                                             
         MVI   ELCODE,X'13'                                                     
         B     DR38                                                             
         CLI   SPCMED,C'X'                                                      
         BNE   DR38                                                             
         MVI   ELCODE,X'14'                                                     
DR38     MVC   CGELCODE,ELCODE          SAVE ELCODE FOR NEXTEL                  
         BAS   RE,GETEL                 ANY CLIENT GROUPS?                      
         BNE   DR39                     NO, CHECK CLIENTS                       
         MVC   SPCSTRT,CGHEAD           SHOW CLIENT GROUP HEADER                
         OI    SPCSTRTH+6,X'80'                                                 
         B     *+12                                                             
*                                                                               
DR38A    BAS   RE,NEXTEL                                                        
         BNE   DR100                                                            
*                                                                               
         L     R1,=A(SPCGRTAB)                                                  
         A     R1,RELO                                                          
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
DR38AA   CLC   SPVCGRID,2(R1)                                                   
         BE    DR38AAA                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,DR38AA                                                        
         DC    H'0'                                                             
DR38AAA  MVC   CGFLT,SPVCGRF                                                    
         MVC   CGID,0(R1)                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,SPVCGRP,CGCD,2,=C'TOG'                                 
         DROP  RF                                                               
*                                                                               
* SAVE PTR TO CLTGRP ELEM FOR NEXTEL                                            
*                                                                               
         LR    R5,R6                                                            
*                                                                               
* READ CLIENT GROUP REC TO GET LOWEST BREAK NAME                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
         MVI   GRPKTYP,GRPKTYPQ         X'0D'                                   
         MVI   GRPKSTYP,GRPKCTYQ        X'04'                                   
         MVC   GRPKAGMD,BAGYMD          A/M                                     
         MVC   GRPKID,SPVCGRID          CLIENT GROUP ID                         
         MVC   GRPKCODE,SPVCGRP         CLIENT GROUP CODE                       
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2                 DONT CLOBBER SUPV RECORD                
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVC   CGNAME(5),=C'?????'                                              
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR38B                                                            
         OC    GRPGNAM1,GRPGNAM1        DONT MOVE IN BLANKS                     
         BZ    DR38B                                                            
         MVC   CGNAME,GRPGNAM1                                                  
         OC    GRPGNAM2,GRPGNAM2        DONT MOVE IN BLANKS                     
         BZ    DR38B                                                            
         CLC   GRPGNAM1,GRPGNAM2        NAME1 < NAME2                           
         BL    DR38B                    YES                                     
         MVC   CGNAME,GRPGNAM2                                                  
         DROP  R4,R6                                                            
*                                                                               
* READ GROUP ID DEFINITION RECORD TO DISPLAY ACCORDING TO BREAK LENGTHS         
*                                                                               
DR38B    LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(GRPKCODE-GRPKEY),KEYSAVE                                     
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2                 DONT CLOBBER SUPV RECORD                
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ          BREAK DESCRIPTION                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING GRPBRKD,R6                                                       
         LLC   R4,GRPBK1LN              BREAK 1 LENGTH                          
         LLC   R1,GRPBK2LN              BREAK 2 LENGTH                          
         AR    R4,R1                    ADD THEM                                
         LA    R1,CGCD+3                4TH DIGIT OF GROUP CODE                 
         DROP  R6                                                               
*                                                                               
DR38C    CHI   R4,4                     DISPLAYED CORRECT LENGTH?               
         BE    DR38D                    YES                                     
         MVI   0(R1),X'40'              CLEAR EXTRA DIGIT                       
         BCTR  R1,0                     POINT TO PREVIOUS DIGIT                 
         AHI   R4,1                     UNTIL BREAK LENGTH IS 4                 
         B     DR38C                                                            
*                                                                               
DR38D    MVC   AIO,AIO1                                                         
         MVC   ELCODE,CGELCODE          RESTORE CLIENT GROUP ELCODE             
         LR    R6,R5                    RESTORE PTR TO CGRP ELEM                
*                                                                               
         LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'              XMIT                                    
         LA    R3,LSDLQ(R3)             POINT TO NEXT SCREEN FIELD              
         LA    R1,SPCENDH                                                       
         CR    R3,R1                    END OF SCREEN?                          
         BNH   DR38A                    NO, CONTINUE DISPLAYING                 
         B     DR100                    YES, DONE                               
         DROP  R3                                                               
***                                                                             
* DISPLAY CLIENTS ACCORDING TO PASSIVE KEYS (ALREADY CHECKED CGROUPS)           
***                                                                             
DR39     LA    R4,KEY                   USE PAS KEY TO SUPV BY CLIENT           
         USING SPVPASS,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,STARTCLT                                                 
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         USING LISD,R3                                                          
         LA    R3,SPCSELH               POINT TO 1ST SELECT FIELD               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     DR45                                                             
*                                                                               
DR40     MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
DR42     GOTO1 SEQ                                                              
*                                                                               
DR45     CLC   SAVEKEY2(5),KEY                                                  
         BNE   DR100                                                            
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         CLC   SPVPSPV,SUPV             SUPV IN KEY = INPUT SUPV?               
         BNE   DR42                     NO, CHECK NEXT RECORD                   
*                                                                               
         MVC   CLC,SPVPCLT              DISPLAY CLIENT CODE                     
**                                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING SPVCLTD,R6                                                       
         MVI   ELCODE,X'02'                                                     
         CLI   SPCMED,C'R'                                                      
         BNE   *+12                                                             
         MVI   ELCODE,X'03'                                                     
         B     DR46                                                             
         CLI   SPCMED,C'X'                                                      
         BNE   DR46                                                             
         MVI   ELCODE,X'04'                                                     
DR46     BAS   RE,GETEL                                                         
DR46A    CLC   SPVPCLT,SPVCLT                                                   
         BE    DR46B                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR46A                                                            
         DC    H'0'                                                             
DR46B    MVC   CLFLT,SPVFILT            OUTPUT FILTER                           
         DROP  R6                                                               
*                                                                               
         LA    R1,SPCSELH               ARE WE AT FIRST SELECT FIELD?           
         CR    R3,R1                                                            
         BNE   *+10                     NO, SKIP                                
         MVC   FRSTCLT,SPVPCLT          YES, STORE FIRST CLIENT                 
*                                                                               
         MVC   LASTCLT,SPVPCLT          STORE LAST CLIENT EVERY TIME            
*                                                                               
         XC    TEMPFLD,TEMPFLD          BUILD FAKE SCREEN FIELDHEADER           
         MVC   TEMPFLD(8),=X'0000000000030000'                                  
         MVC   TEMPFLD+8(3),CLC         PUT ONSCREEN CLIENT IN FIELD            
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,3               PRESERVE AIO AND AIO2                   
         GOTO1 VALICLT                  VALICLT TO GET CLIENT NAME              
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
*                                                                               
         MVC   CLNM,CLTNM               DISPLAY CLIENT NAME                     
         LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'                                                      
*                                                                               
         LA    R3,LSDLQ(R3)             POINT TO NEXT SCREEN FIELD              
         LA    R1,SPCENDH                                                       
         CR    R3,R1                    END OF SCREEN?                          
         BNH   DR40                     NO, CONTINUE DISPLAYING                 
         B     DR100                    YES, DONE                               
*                                                                               
* LIST BY BUYER                                                                 
*                                                                               
DR50     DS    0H                                                               
         CLI   8(R2),C'B'                                                       
         BNE   DR80                                                             
         TM    4(R2),X'80'              "LIST BY" INPUT THIS TIME?              
         BNO   DR51                     NO, DISPLAY FROM LAST POS               
         XC    STARTBYR,STARTBYR        YES, DISPLAY FROM FIRST BUYER           
         XC    FRSTBYR,FRSTBYR                                                  
         XC    LASTBYR,LASTBYR                                                  
*                                                                               
DR51     CLI   5(R2),0                  MEDIA INPUT AT ALL?                     
         BNE   DR52                     YES, FIND ELEM CODE FOR MEDIA           
         MVI   8(R2),C'T'               NO, DEFAULT TO TELEVISION               
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR52     LA    R4,MEDTAB                                                        
*                                                                               
DR53     CLC   SPCMED,0(R4)                                                     
         BE    DR54                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR53                                                             
         B     ERRINV                                                           
*                                                                               
DR54     MVC   MEDELTX,0(R4)            ELCODE FOUND                            
*                                                                               
         BRAS  RE,CLBTSC                CLEAR BOTTOM PORTION OF SCREEN          
*                                                                               
         L     R1,=A(BUHEAD)                                                    
         A     R1,RELO                                                          
         MVC   SPCSTRT,0(R1)            SHOW BUYER HEADER                       
         OI    SPCSTRTH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY                  BUILD P-KEY TO BUYER BY SUPV            
         USING BYRRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'                                                
         MVC   BYRPAGY2,AGYX                                                    
         MVC   BYRPOFC2,OFFICE                                                  
         MVC   BYRPSPV2,SUPV            USE INPUT SUPV                          
*                                                                               
* PFKEY STUFF                                                                   
         CLI   PFKEY,8                  DOWN?                                   
         BNE   DR55                     NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTBYR,FRSTBYR          YES,DON'T PAGE DOWN                     
         MVC   STARTBYR,LASTBYR                                                 
         B     DR60                                                             
*                                                                               
DR55     CLI   PFKEY,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTBYR,FRSTBYR          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTBYR,FRSTBYR         IF ANY OTHER PFKEY,START                
*                                                                               
DR60     XC    FRSTBYR,FRSTBYR                                                  
         XC    LASTBYR,LASTBYR                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTBYR,STARTBYR        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         MVC   BYRPBYR2,STARTBYR                                                
*                                                                               
         USING LISD,R3                                                          
         LA    R3,SPCSELH              POINT TO FIRST SELECT FIELD              
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     DR75                                                             
DR70     GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
DR75     CLC   SUPV,BYRPSPV2            IS SUPV IN KEY = INPUT SUPV?            
         BNE   DR100                    NO, NO BUYERS FOUND                     
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING BYRRECD,R4                                                       
         L     R4,AIO                                                           
         MVC   BYRC,BYRKBYR             DISPLAY BUYER CODE                      
         DROP  R4                                                               
*                                                                               
         LA    R1,SPCSELH               ARE WE AT FIRST SELECT FIELD?           
         CR    R3,R1                                                            
         BNE   *+10                     NO, SKIP                                
         MVC   FRSTBYR,BYRC             YES, STORE FIRST BUYER                  
*                                                                               
         MVC   LASTBYR,BYRC             STORE LAST BUYER EVERY TIME             
*                                                                               
         USING BYRNAMED,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'             GET BUYER NAME ELEMENT                  
         BAS   RE,GETEL                                                         
*                                                                               
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'BYRLNAME),BYRLNAME                                       
         MVC   BLOCK+15(L'BYRFNAME),BYRFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'BYNM)                                
         MVC   BYNM,BLOCK               PRINT BUYER NAME ELEM                   
         LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'                                                      
         DROP  R3                                                               
*                                                                               
         LA    R3,LSDLQ(R3)             POINT TO NEXT SCREEN FIELD              
         LA    R1,SPCENDH               END OF SCREEN?                          
         CR    R3,R1                                                            
         BNH   DR70                     NO, CONTINUE DISPLAYING                 
         B     DR100                    YES, DONE                               
*                                                                               
* LIST BY MARKET                                                                
*                                                                               
DR80     DS    0H                                                               
         CLI   8(R2),C'M'               IF "LIST BY" NOT = C,B, OR M -          
         BNE   ERRINV                   - INPUT IS INVALID                      
         TM    4(R2),X'80'              WAS THERE INPUT THIS TIME?              
         BNO   DR81                     NO, START DISPLAY FROM LAST POS         
         XC    STARTMKT,STARTMKT        YES,DISPLAY FROM FIRST MARKET           
         XC    LASTMKT,LASTMKT                                                  
         XC    FRSTMKT,FRSTMKT                                                  
*                                                                               
DR81     BAS   RE,NOSELECT                                                      
         BRAS  RE,CLBTSC                                                        
         L     R1,=A(MKHEAD)                                                    
         A     R1,RELO                                                          
         MVC   SPCSTRT,0(R1)            SHOW MARKET HEADER                      
         OI    SPCSTRTH+6,X'80'                                                 
*                                                                               
         LA    R2,SPCMEDH                                                       
         TM    4(R2),X'80'              MEDIA INPUT THIS TIME?                  
         BNO   DR81A                    SAME AS FOR "LIST BY" INPUT             
         XC    STARTMKT,STARTMKT                                                
         XC    LASTMKT,LASTMKT                                                  
         XC    FRSTMKT,FRSTMKT                                                  
*                                                                               
DR81A    CLI   5(R2),0                  MEDIA INPUT AT ALL?                     
         BNE   DR82                     YES, FIND ELEM CODE FOR MEDIA           
         MVI   8(R2),C'T'               NO, DEFAULT TO TELEVISION               
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR82     LA    R4,MEDTAB                                                        
*                                                                               
DR84     CLC   SPCMED,0(R4)                                                     
         BE    DR86                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DR84                                                             
         B     ERRINV                                                           
*                                                                               
DR86     MVC   MEDELCD,1(R4)            ELCODE FOUND                            
*                                                                               
**** MAKE VALIMED CALL SO CAN MAKE VALIMKT CALL ****                            
         MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         GOTO1 VALIMED                  MAKES A DM CALL                         
*                                                                               
         XC    KEY,KEY                  BUILD PAS KEY TO BUYER VIA SUPV         
         USING BYRRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'                                                
         MVC   BYRPAGY2,AGYX                                                    
         MVC   BYRPOFC2,OFFICE                                                  
         MVC   BYRPSPV2,SUPV            LOOK FOR P-KEY WITH INPUT SUPV          
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         MVC   BYRTAB,SPACES            CLEAR BUYER TABLE                       
         LA    R3,BYRTAB                                                        
         GOTO1 HIGH                                                             
         B     DR90                                                             
DR88     GOTO1 SEQ                                                              
DR90     CLC   KEY(9),SAVEKEY2          END OF 0DE3 RECORDS?                    
         BNE   DR92                     YES, GET PAS KEY TO BYR/MKT             
         MVC   0(L'STARTBYR,R3),BYRPBYR2   STORE IN BUYER TABLE                 
         LA    R3,L'STARTBYR(R3)                                                
         LA    R0,BYTBEND                                                       
         CR    R3,R0                    REACHED END OF TABLE?                   
         BL    DR88                     NO, KEEP READING                        
         DROP  R4                                                               
*                                                                               
DR92     LA    R3,SPCSELH                                                       
         USING LISD,R3                                                          
*                                                                               
         CLI   PFKEY,8                  DOWN?                                   
         BNE   DR92A                    NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTMKT,FRSTMKT          YES,DON'T PAGE DOWN                     
         MVC   STARTMKT,LASTMKT                                                 
         B     DR92B                                                            
*                                                                               
DR92A    CLI   PFKEY,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTMKT,FRSTMKT          START AT FIRST MARKET KEY               
*                                                                               
         MVC   STARTMKT,FRSTMKT         IF ANY OTHER PFKEY,START                
*                                                                               
DR92B    XC    FRSTMKT,FRSTMKT                                                  
         XC    LASTMKT,LASTMKT                                                  
*                                                                               
         TM    STATUS,KEYCHG                                                    
         BNO   *+10                                                             
         XC    STARTMKT,STARTMKT                                                
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
         XC    KEY,KEY                  BUILD PAS KEY TO BUYER VIA MKT          
         USING BYRRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,STARTMKT                                                 
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     DR98                                                             
DR96     MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
DR98     CLC   KEY(5),SAVEKEY2                                                  
         BNE   DR100                                                            
*                                                                               
         MVC   SAVEKEY2,KEY             FOR DM SEQUENCING                       
*                                                                               
         CLC   KEY+5(2),LASTKEY+5       COMPARE MKTS                            
         BNE   *+14                                                             
         CLC   KEY+7(4),LASTKEY+7       MKTS SAME, COMPARE BUYERS               
         BNE   DR96                     DIFFERENT BUYERS, DON'T DISPLAY         
*                                                                               
         LA    R5,BYRTAB                POINT TO BEG. OF BUYER TABLE            
DR99     CLC   BYRPBYR,0(R5)            BUYERS FOUND?                           
         BE    DR99A                    YES, DISPLAY BUYER CODE                 
         LA    R5,L'STARTBYR(R5)        NO, CHECK NEXT TABLE ENTRY              
         LA    R0,BYTBEND                                                       
         CR    R5,R0                    REACHED END OF TABLE?                   
         BNL   DR96                                                             
         B     DR99                                                             
*                                                                               
DR99A    MVC   MKBYCD,BYRPBYR           DISPLAY BUYER CODE FOR MKT              
         MVC   LASTKEY,KEY              SAVE KEY OF PREVIOUS RECORD             
*                                                                               
         CLC   =X'FFFF',BYRPMKT         FOR "ALL MARKETS"                       
         BNE   DR99B                                                            
         MVC   MKTC,=C'ALL '                                                    
         MVC   MKNM,=C'ALL MARKETS'                                             
         B     DR99C                    SKIP REST OF VALIMKT PROCEDURE          
*                                                                               
DR99B    ZICM  R0,BYRPMKT,2                                                     
         CVD   R0,DUB                                                           
         UNPK  MKTC,DUB                                                         
         OI    MKTC+3,X'F0'             DISPLAY MARKET CODE                     
*                                                                               
         LA    R1,SPCSELH               FIRST OUTPUT FIELD?                     
         CR    R3,R1                                                            
         BNE   *+10                     NO, KEEP GOING                          
         MVC   FRSTMKT,BYRPMKT          YES, STORE FIRST MARKET                 
*                                                                               
         MVC   LASTMKT,BYRPMKT          STORE LAST MARKET EVERY TIME            
         DROP  R4                                                               
*                                                                               
         XC    TEMPFLD,TEMPFLD          BUILD FAKE HEADER/SCREEN FIELD          
         MVC   TEMPFLD(8),=X'0000000008040000'                                  
         MVC   TEMPFLD+8(4),MKTC                                                
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                  TO GET MARKET NAME                      
         MVC   AIO,AIO2                                                         
*                                                                               
         MVC   MKNM,MKTNM               DISPLAY MARKET NAME                     
DR99C    LA    R1,LSDSLQ(R3)                                                    
         OI    6(R1),X'80'                                                      
*                                                                               
         LA    R3,LSDLQ(R3)             POINT TO NEXT SCREEN FIELD              
         LA    R1,SPCENDH                                                       
         CR    R3,R1                    END OF SCREEN?                          
         BNH   DR96                     NO, CONTINUE DISPLAY                    
*                                                                               
DR100    CLI   ACTEQU,ACTSEL            ACTION SELECT?                          
         BNE   DR100A                   NO, CHECK FOR ACTION CHANGE             
         CLI   SAVESEL,C'C'             YES,DON'T EXIT YET                      
         BE    DR101                                                            
         CLI   SAVESEL,C'D'             DELETE ON FROM LIST?                    
         BE    DR101                                                            
DR100A   CLI   ACTEQU,ACTCHA                                                    
         BNE   DRX                      EXIT IF NOT ACT CHA OR SEL              
DR101    MVC   KEY,SAVEKEY              RESTORE KEY                             
         MVC   AIO,AIO2                 DON'T BLOW AWAY AIO1                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                   GET D/A OF REC FOR PUTREC               
DRX      MVC   AIO,AIO1                 RESTORE CORRECT AIO FOR PUTREC          
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         BAS   RE,CHKSEL                CHECK SELECT FIELDS                     
         NI    STATUS,X'FF'-KEYCHG      TURN OFF KEYCHANGE BIT                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING SPVRECD,R6                                                       
*                                                                               
         MVC   OFFICE,SPVKOFC                                                   
         OC    OFFICE,SPACES                                                    
         MVC   SPCOFF,OFFICE                                                    
         OI    SPCOFFH+6,X'80'                                                  
*                                                                               
         MVC   SUPV,SPVKSPV                                                     
         OC    SUPV,SPACES                                                      
         MVC   SPCSUP,SUPV                                                      
         OI    SPCSUPH+6,X'80'                                                  
         MVC   SAVEKEY,0(R6)            SAVE THE KEY                            
*                                                                               
DKX      CLI   ACTEQU,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*ONLINE LIST                                                                    
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING SPVRECD,R6                                                       
         OC    KEY,KEY        IS THIS FIRST TIME AT LIST SCREEN?                
         BNZ   LR10           NO, DO NOT BUILD KEY                              
*                                                                               
         MVC   KEY,SAVEKEY                                                      
*                                                                               
LR10     GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                FIND SUBSEQUENT DIRECTORY RECS                
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(3),SAVEKEY     IF PAST SUPERVISOR/AGY RECS                   
         BNE   LRX                STOP READING RECORDS                          
*                                                                               
         GOTO1 GETREC             GET SUPERVISOR DATA                           
*                                                                               
         MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
         L     R6,AIO                                                           
         USING SPVRECD,R6                                                       
         MVC   LSOFFCD,SPVKOFC                                                  
         MVC   LSSUPCD,SPVKSPV    PRINT SUPERVISOR CODE                         
*                                                                               
         MVI   ELCODE,X'01'       FIND SUPERVISOR NAME ELEMENT                  
         BAS   RE,GETEL                                                         
*                                                                               
         USING SPVNAMED,R6                                                      
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'SPVLNAME),SPVLNAME                                       
         MVC   BLOCK+15(L'SPVFNAME),SPVFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'LSSUPNM)                             
         MVC   LSSUPNM,BLOCK               PRINT BUYER NAME ELEM                
*                                                                               
         TM    FILTFLAG,FILTCLTQ           IS THERE CLIENT FILTER?              
         BNO   LR35                                                             
         BAS   RE,FLTCLT                   FILTER BY CLIENT                     
         BNE   LR20                                                             
*                                                                               
LR35     TM    FILTFLAG,FILTPNYQ+FILTPNNQ  FILTER OUT A PLAN?                   
         BZ    LR40                        NOPE                                 
         BAS   RE,FLTPLAN                  WANT TO FILTER THIS ONE OUT?         
         BNE   LR20                        YES                                  
*                                                                               
LR40     DS    0H                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*** ADD AND DELETE PASSIVE POINTERS                                             
*                                                                               
XR       DS    0H                                                               
         CLI   ACTEQU,ACTREST           ACTION RESTORE?                         
         BE    XR7A                     YES, ADD BACK P-KEYS                    
         CLI   SAVESEL,C'D'             IS SELECT = DELETE?                     
         BE    XR3                      YES, DELETE P-KEYS                      
         CLI   ACTEQU,ACTDEL            ACTION DELETE?                          
         BNE   XR9                      NO, GOTO ROUTINE FOR ADD/CHANGE         
*                                                                               
* FOR XRECDELETE                                                                
         USING SPVRECD,R4                                                       
XR3      XC    KEY,KEY                  DELETE CLT P-KEYS                       
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'        SEARCHING FOR ALL CLIENT -               
         MVC   SPVPAM,AGYX            - P-KEYS TO THE BUYER FOR -               
         MVC   SPVPOFC,OFFICE          - ALL MEDIA                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR5                                                              
*                                                                               
XR4      GOTO1 SEQ                                                              
XR5      CLC   KEY(2),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XRX                   NO, DONE                                   
*                                                                               
         CLC   SPVPOFC,OFFICE           CORRECT OFF?                            
         BH    XRX                      HIGHER, EXIT                            
         CLC   SPVPSPV,SUPV            CORRECT SUPV?                            
         BNE   XR4                     NO, CHECK NEXT                           
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR4                      CHECK NEXT RECORD                       
*                                                                               
* FOR XRECRESTORE                                                               
* NOTE: THIS WILL NOW (10/01/02) RESTORE PASSIVE KEYS BASED ON THE CLT          
*       ELEMENTS RATHER THAN RESTORE ALL THE PASSIVE KEYS - FOOL!               
*                                                                               
XR7A     L     R6,AIO                                                           
         CLC   0(2,R6),=X'0D61'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,24(R6)            FIRST ELEMENT                               
*                                                                               
XR7B     CLI   0(R6),0              END OF RECORD?                              
         BE    XRX                  YES - DONE                                  
         CLI   0(R6),X'02'          LOOKING FOR X'02' - X'04' ELEMENTS          
         BL    XR7C                                                             
         CLI   0(R6),X'04'                                                      
         BNH   XR7D                                                             
*                                                                               
XR7C     LLC   R0,1(R6)             BUMP TO NEXT ELEMENT                        
         AR    R6,R0                                                            
         B     XR7B                                                             
*                                                                               
         USING SPVCLTD,R6                                                       
XR7D     MVC   AIO,AIO1                ACTIVE KEY IN AIO1                       
         L     R5,AIO                  GET INFO OFF ACTIVE KEY                  
         MVC   AIO,AIO2                GET PASSIVE KEY IN AIO2                  
         XC    KEY,KEY                 ADD CLT P-KEYS                           
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'        SEARCHING FOR ALL CLIENT -               
         MVC   SPVPAM,BAGYMD           A/M                                      
         NI    SPVPAM,X'F0'            TURN OFF MEDIA BIT                       
         CLI   0(R6),X'02'             MEDIA T?                                 
         BNE   *+8                     NO                                       
         OI    SPVPAM,X'01'            YES - TURN ON MEDIA T BIT                
         CLI   0(R6),X'03'             MEDIA R?                                 
         BNE   *+8                     NO                                       
         OI    SPVPAM,X'02'            YES - TURN ON MEDIA R BIT                
         CLI   0(R6),X'04'             MEDIA X?                                 
         BNE   *+8                     NO                                       
         OI    SPVPAM,X'04'            YES - TURN ON MEDIA X BIT                
*                                                                               
         MVC   SPVPOFC,SPVKOFC-SPVKEY(R5)  OFFICE CODE FROM ACTIVE KEY          
         MVC   SPVPCLT,SPVCLT          CLT FROM ELEM OF ACTIVE KEY              
         MVC   SPVPSPV,SPVKSPV-SPVKEY(R5)  SUPERVISOR FROM ACTIVE KEY           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ DELETED RECORDS                     
         GOTO1 HIGH                                                             
XR8A     CLC   KEY(SPVPSPV-SPVPASS+L'SPVPSPV),KEYSAVE  PASV KEY FOUND?          
         BE    *+6                     YES                                      
         DC    H'0'                                                             
*                                                                               
         NI    KEY+13,X'FF'-X'80'      TURN OFF DELETE BIT                      
         GOTO1 WRITE                   RESTORE P-KEY                            
         B     XR7C                    CHECK NEXT ELEM                          
         DROP  R4,R6                                                            
*                                                                               
* FOR XRECADD AND XRECPUT                                                       
XR9      DS    0H                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         MVC   DISKADD,KEY+14                                                   
*                                                                               
         LA    R3,DELCLTS                                                       
*                                                                               
         USING SPVRECD,R4                                                       
XR10     OC    0(L'DELCLTS,R3),0(R3)                                            
         BZ    XR30                                                             
*                                                                               
         XC    KEY,KEY                  DELETE CLIENT POINTERS                  
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,0(R3)                                                    
         MVC   SPVPSPV,SUPV                                                     
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVPASS),KEYSAVE    PASSIVE POINTER FOUND?                 
         BNE   XR15                     NO, CHECK NEXT DELETED CLIENT           
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                                                            
*                                                                               
XR15     LA    R3,L'DELCLTS(R3)                                                 
         LA    R0,DECLEND                                                       
         CR    R3,R0                                                            
         BNE   XR10                                                             
*                                                                               
XR30     DS    0H                       DELETE CLT PTR'S TO OTHER SPV'S         
         LA    R3,ADDCLTS                                                       
*                                                                               
XR35     OC    0(L'ADDCLTS,R3),0(R3)                                            
         BZ    XRX                                                              
*                                                                               
* IF 'ALLOW MULTIPLE ASSIGNMENTS' OPTION IN SD PROFILE IS 'Y'                   
* DON'T DELETE PASSIVE POINTERS                                                 
*                                                                               
         CLI   SDPROF,C'Y'                                                      
         BE    XR50                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,0(R3)                                                    
*                                                                               
         MVC   AIO,AIO2                 PRESERVE D/A OF INPUT REC               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR38                                                             
*                                                                               
XR36     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
XR38     DS    0H                                                               
         CLC   KEY(8),KEYSAVE           PREVIOUS PASSIVE POINTER FOUND?         
         BNE   XR50                     NO, ADD THE CLIENT                      
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         USING SPVCLTD,R6                                                       
XR40     BAS   RE,NEXTEL                ELEM FOUND?                             
         BNE   XR50                     NO, ADD NEW PASSIVE POINTER             
         CLC   SPVCLT,0(R3)                                                     
         BNE   XR40                                                             
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                   WRITE RECORD W/ DELETED CLIENTS         
         B     XR36                                                             
*                                                                               
XR50     XC    KEY,KEY                  ADD PASSIVE POINTER FOR CLIENT          
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,0(R3)                                                    
         MVC   SPVPSPV,SUPV                                                     
         MVC   KEY+14(4),DISKADD       DISK ADDRESS OF RECORD IN VR             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ FOR DELETED RECORDS                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVPASS),KEYSAVE                                           
         BE    XR60                                                             
         MVC   KEY(18),KEYSAVE          13 FOR KEY, 1 STATUS, 4 D/A             
         GOTO1 ADD                      NO RECORD FOUND, SO ADD P PTR           
         B     XR65                                                             
*                                                                               
XR60     NI    KEY+13,X'FF'-X'80'       UNDELETE OLD PASSIVE POINTER            
         GOTO1 WRITE                                                            
*                                                                               
XR65     LA    R3,L'ADDCLTS(R3)                                                 
         LA    R0,ADCLEND                                                       
         CR    R3,R0                                                            
         BNE   XR35                     CHECK NEXT ADDED CLIENT                 
*                                                                               
XRX      MVC   KEY(13),SAVEKEY                                                  
         MVC   AIO,AIO1                 RESTORE DISKADD FOR DISPLAY             
         MVI   RDUPDATE,C'N'            DON'T READ DELETED REC'S -              
         NI    DMINBTS,X'FF'-X'08'      - IN NEXT CALL TO DISPLAY               
         CLI   ACTEQU,ACTDEL                                                    
         BNE   DR                       DISPLAY AFTER CHANGE PASS KEYS          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* SPECIAL CASE FOR DELETING SUPERVISOR RECORDS                                  
*                                                                               
RCDL     DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECS                 
*                                                                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE                                                   
         MVC   SAVEKEY2,KEY                                                     
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     RCDL10                                                           
*                                                                               
RCDL5    GOTO1 SEQ                                                              
RCDL10   CLC   SAVEKEY2(5),KEY                                                  
         BNE   RCDLX                                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'             BUYER NAME ELEMENT                      
         BAS   RE,GETEL                                                         
*                                                                               
         USING BYRNAMED,R6                                                      
         CLC   BYRSPV,SUPV                                                      
         BE    ERRBYR                                                           
         DROP  R6                                                               
*                                                                               
         B     RCDL5                                                            
*                                                                               
RCDLX    MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
ERRELEM  MVC   ERRNUM,=AL2(MISSEL)                                              
         B     SPERREX                                                          
ERRBYR   MVC   ERRNUM,=AL2(DELBYR)                                              
         B     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(DUPCL)                                               
         B     SPERREX                                                          
ERRNOOFF MVC   ERRNUM,=AL2(NOOFF)                                               
         B     SPERREX                                                          
ERRCLTEX MVC   ERRNUM,=AL2(CLTEX)                                               
         B     SPERREX                                                          
ERRNOCLT MVC   ERRNUM,=AL2(NOCLT)                                               
         B     SPERREX                                                          
ERRMIX   MVC   ERRNUM,=AL2(1147)    MUST DEL CLT BEFORE ADDING CGRP             
         B     SPERREX                                                          
ERRDUPCG MVC   ERRNUM,=AL2(1148)    DUPLICATE CLIENT GROUP                      
         B     SPERREX                                                          
ERRCGRP  MVC   ERRNUM,=AL2(1149)    INVALID CLIENT GROUP                        
         B     SPERREX                                                          
ERRALPH  MVC   ERRNUM,=AL2(1150)    CLIENT GROUP ID MUST BE ALPHABETIC          
         B     SPERREX                                                          
ERRNUMB  MVC   ERRNUM,=AL2(1151)    CLIENT GROUP CODE MUST BE NUMERIC           
         B     SPERREX                                                          
ERRCGNF  MVC   ERRNUM,=AL2(1153)    CLIENT GROUP RECORD DOES NOT EXIST          
         B     SPERREX                                                          
ERRCGREX MVC   ERRNUM,=AL2(1154)    CLIENT GROUP ALREADY IN RECORD              
         B     SPERREX                                                          
ERRMIX2  MVC   ERRNUM,=AL2(1155)    MUST DEL CLT BEFORE ADDING CGRP             
         B     SPERREX                                                          
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
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
*                                                                               
NOCLT    EQU   423                      CLIENT NOT FOUND ERROR MESSAGE          
CLTEX    EQU   431                      CLIENT EXISTS ALREADY                   
NOOFF    EQU   432                                                              
DUPCL    EQU   436                                                              
DELBYR   EQU   438                                                              
MISSEL   EQU   471                                                              
*                                                                               
         SPACE 2                                                                
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
**** ROUTINE TO VALIDATE OFFICE CODE ****                                       
*                                                                               
VALOFF   NTR1                                                                   
         CLI   ACTEQU,ACTADD                                                    
         BNE   VLOFX                                                            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD OFFICE KEY                             
         USING OFCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   OFCKTYP,=X'0D60'                                                 
         MVC   OFCKAGY,AGYX                                                     
         MVC   OFCKOFC,OFFICE      OFFICE CODE                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    VLOFX                                                            
*                                                                               
         LA    R2,SPCOFFH                                                       
         B     ERRNOOFF                                                         
VLOFX    B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SEE IF ITS A CLIENT GROUP (CC EQU IF A VALID CLTGRP ELSE CC NEQU)             
* ENTRY - R2 TO FIELD                                                           
* EXIT  - CGROUPID SET WITH XL1 ID AND CGROUPCD WITH XL2 CODE                   
*******************************************************************             
ISVALCGR NTR1                                                                   
*                                                                               
         CLI   8(R2),C'-'               STARTS WITH A MINUS?                    
         BNE   VC10                     NOPE                                    
         CLI   5(R2),6                  IS IT A CLIENT GROUP?                   
         BE    VC05                     NO                                      
         CLI   5(R2),7                  IS IT A CLIENT GROUP?                   
         BE    VC05                     NO                                      
         CLI   5(R2),8                  IS IT A CLIENT GROUP?                   
         BE    VC05                     NO                                      
         CLI   5(R2),9                  IS IT A CLIENT GROUP?                   
         BNE   XNO                      NO                                      
VC05     LA    R1,9(R2)                 POINT TO 1ST CHAR OF CLTGRP             
         B     VC15                                                             
*                                                                               
VC10     CLI   5(R2),6                  IS IT A CLIENT GROUP?                   
         BE    VC13C                    NO                                      
         CLI   5(R2),7                  DON'T FORGET 2 CHAR GROUPS?             
         BE    VC13C                    NO                                      
         CLI   5(R2),8                  DON'T FORGET 2 CHAR GROUPS?             
         BNE   XNO                      NO                                      
VC13C    LA    R1,8(R2)                 POINT TO 1ST CHAR OF CLTGRP             
***      CLI   3(R1),C'/'               CLIENT WITH A FILER?                    
***      BE    XNO                      YES, NOT A CLIENT GROUP                 
*                                                                               
VC15     MVC   WORK(2),0(R1)            POINT TO GROUP CODE                     
         CLI   WORK+1,C'0'                                                      
         BL    *+8                                                              
         MVI   WORK+1,C' '                                                      
*                                                                               
         L     R3,=A(SPCGRTAB)                                                  
         A     R3,RELO                                                          
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
VC17     CLC   WORK(2),0(R3)                                                    
         BE    VC19                                                             
         LA    R3,3(R3)                                                         
         BCT   R0,VC17                                                          
         B     ERRALPH                                                          
*                                                                               
VC19     MVC   CGROUPID,2(R3)            SAVE 1 CHAR GROUP ID                   
*                                                                               
VC20     LA    R1,1(R1)                 NEXT 4 BYTES MUST BE NUMERIC            
         CLI   1(R3),C' '          ONE OR 2 CHAR ID                             
         BE    *+8                                                              
         LA    R1,1(R1)                 ASSUME 2 CHAR CLGROUP                   
*                                                                               
         LA    RE,4                                                             
VC25     CLI   0(R1),C'0'                                                       
         BL    ERRNUMB                                                          
         CLI   0(R1),C'9'                                                       
         BH    ERRNUMB                                                          
         LA    R1,1(R1)                 CHECK NEXT CHARACTER                    
         BCT   RE,VC25                                                          
*                                                                               
         MVI   CGROUPF,0           CLEAR OUT THE FILTER VALUE                   
         CLI   0(R1),C'/'          DO WE HAVE A SLASH?                          
         BNE   VC30                 - NOPE                                      
         MVC   CGROUPF,1(R1)       SAVE OFF THE FILTER VALUE                    
*                                                                               
VC30     AHI   R1,-4                                                            
         L     RF,ACOMFACS         SET CGROUPCD                                 
         USING COMFACSD,RF                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         LA    R4,0(R1)                                                         
         GOTO1 (RF),DMCB,0(R4),CGROUPCD,4,=C'TOG'                               
         DROP  RF                                                               
*                                                                               
VCX      B     XYES                                                             
         EJECT                                                                  
***                                                                             
* DELETE THE CLIENT GROUP FROM THE RECORD                                       
***                                                                             
DELCLGRP NTR1                                                                   
*                                                                               
         BAS  RE,VCGREC                VALIDATE CLTGRP RECORD                   
*                                                                               
         USING SPVCGRD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DELCG10  BAS   RE,NEXTEL                                                        
         BNE   ERRNOCLT                                                         
         CLC   SPVCGRID(3),CGROUP                                               
         BNE   DELCG10                                                          
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
DELCGX   B     XIT                                                              
         EJECT                                                                  
***                                                                             
* ADD A CLIENT GROUP TO THE RECORD (IF VALID)                                   
* NOTE: R1 IS POINTING TO THE CLIENT GROUP                                      
***                                                                             
ADDCLGRP NTR1                                                                   
*                                                                               
         BAS  RE,VCGREC                VALIDATE CLTGRP RECORD                   
*                                                                               
         USING SPVCGRD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
ACG10    BAS   RE,NEXTEL                                                        
         BNE   ACG20                                                            
         CLC   CGROUP,SPVCGRID          COMP ID + CODE (L'CGROUP = 3)           
         BE    ERRCGREX                 CLIENT GROUP ALREADY EXISTS             
         B     ACG10                                                            
         DROP  R6                                                               
*                                                                               
ACG20    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING SPVCGRD,R5                                                       
*                                                                               
         MVC   SPVCGREL,MEDELCD         MEDIA CODE                              
         OI    SPVCGREL,X'10'                                                   
         MVI   SPVCGRLN,SPVCGRLQ        LENGTH                                  
         MVC   SPVCGRID,CGROUPID        CLIENT GROUP ID                         
         MVC   SPVCGRP,CGROUPCD         CLIENT GROUP CODE                       
         MVC   SPVCGRF,CGROUPF          CLIENT GROUP FILTER VALUE               
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ACGX     B     XIT                                                              
         EJECT                                                                  
***                                                                             
* READ THE CLIENT GROUP RECORD TO SEE IF IT'S A VALID CLIENT GROUP              
* NOTE: R1 POINTING TO CLIENT GROUP                                             
***                                                                             
VCGREC   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPKEY,R3                                                        
         MVI   GRPKTYP,GRPKTYPQ         X'0D'                                   
         MVI   GRPKSTYP,GRPKCTYQ        X'04'                                   
         MVC   GRPKAGMD,BAGYMD          A/M                                     
         MVC   GRPKID,CGROUPID                                                  
         MVC   GRPKCODE,CGROUPCD                                                
         MVC   AIO,AIO2                 DONT CLOBBER SUPV RECORD                
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BNE   ERRCGNF                                                          
         MVC   AIO,AIO1                 RESTORE SUPV RECORD                     
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO ADD CLIENT A SINGLE CLIENT ELEMENT ****                      
*                                                                               
ADDCLI   NTR1                                                                   
         CLI   5(R2),2                                                          
         BL    ERRINV                                                           
*                                                                               
         CLI   5(R2),5                                                          
         BH    ERRINV                                                           
*                                                                               
         LA    R4,3                                                             
         LA    R5,CLTCODE          IS THE CLIENT CODE A-9?                      
AC01     CLI   0(R5),X'40'         THIRD BYTE CAN ALSO BE A SPACE               
         BE    AC01A                                                            
         CLI   0(R5),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R5),C'9'                                                       
         BH    ERRINV                                                           
         LA    R5,1(R5)                                                         
AC01A    BCT   R4,AC01                                                          
*                                                                               
         LA    R5,8(R2)            TEST FOR FILTERS                             
         CLI   2(R5),C'/'          '/' IN 3RD OR 4TH BYTE                       
         BNE   AC02                                                             
         LA    R5,3(R5)            POINT TO THE FILTER CHAR                     
         B     AC03                                                             
*                                                                               
AC02     CLI   3(R5),C'/'                                                       
         BNE   AC05                NO FILTER                                    
         LA    R5,4(R5)                                                         
*                                                                               
AC03     CLI   0(R5),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R5),C'Z'                                                       
         BH    ERRINV                                                           
         MVC   CLTFILT,0(R5)       STORE FILTER                                 
*                                                                               
         USING SPVCLTD,R6                                                       
AC05     L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
AC10     BAS   RE,NEXTEL                                                        
         BNE   AC20                                                             
         CLC   CLTCODE,SPVCLT                                                   
         BE    ERRCLTEX                 CLIENT ALREADY EXISTS                   
         B     AC10                                                             
         DROP  R6                                                               
*                                                                               
AC20     L     R6,AIO                                                           
         MVI   USEIONUM,2                                                       
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0B00000000030000'                                  
         LR    R5,R2                    BACKUP R2 BEFORE VALICLT                
         LA    R2,TEMPFLD                                                       
         MVC   8(3,R2),CLTCODE                                                  
         OI    TRNSTAT,NOVALERR                                                 
         GOTO1 VALICLT                  ALSO HAS A DM CALL                      
         TM    TRNSTAT,BASERR                                                   
         BNO   AC25                                                             
         MVI   ERROR,INVCLI                                                     
         LR    R2,R5                                                            
         GOTO1 ERREX                                                            
AC25     LR    R2,R5                    RESTORE R2                              
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING SPVCLTD,R5                                                       
*                                                                               
         MVC   SPVCLTEL,MEDELCD    ELEMENT CODE                                 
         MVI   SPVCLTLN,SPVCLTLQ   ELEMENT LENGTH                               
         MVC   SPVCLT,CLTCODE                                                   
         OC    SPVCLT,SPACES                                                    
         MVC   SPVFILT,CLTFILT                                                  
         XC    CLTFILT,CLTFILT     CLEAR TEMP FILTER                            
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ACX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO DELETE A SINGLE CLIENT ****                                  
*                                                                               
DELCLI   NTR1                                                                   
         USING SPVCLTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DEL10    BAS   RE,NEXTEL                                                        
         BNE   ERRNOCLT                                                         
         CLC   CLTCODE,SPVCLT                                                   
         BNE   DEL10                                                            
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO CHANGE SELECT FIELDS TO PROTECTED FIELDS ****                
*                                                                               
NOSELECT NTR1                                                                   
         LA    R2,SPCSELH                                                       
*                                                                               
NS10     OI    6(R2),X'20'              CHANGE TO PROTECTED                     
         OI    6(R2),X'80'              TRANSMIT                                
*                                                                               
         BAS   RE,NXTSCRF               R2 POINTS TO DATA FIELD                 
         LA    R0,SPCENDH                                                       
         CR    R2,R0                    END OF SCREEN?                          
         BE    NSX                      YES,EXIT                                
*                                                                               
         BAS   RE,NXTSCRF               R2 POINTS TO NEXT SELECT FIELD          
         B     NS10                                                             
*                                                                               
NSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CLEAR TOP OF SCREEN - CLEARS LIST HEADER & ADD CLIENT FIELDS           
***********************************************************************         
*                                                                               
CLTPSC   NTR1  CLEARS              CLEARS SCREEN FROM ADD CLI DOWN              
         LA    R2,SPCFCLH         1ST LINE                                      
         LA    R3,SPCSTRTH                                                      
*                                                                               
CLTPSC10 LLC   R1,0(R2)            FIELD LENGTH                                 
         SHI   R1,9                8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
CLTPSC20 LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLTPSC10            NO                                           
CTSX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
**** SUBROUTINE TO CHECK SELECT FIELDS FOR INPUT ***                            
*                                                                               
CHKSEL   NTR1                                                                   
         LA    R2,SPCSELH                                                       
         TM    6(R2),X'20'                                                      
         BO    CSX                                                              
*                                                                               
CS10     CLI   5(R2),0                                                          
         BE    CS20                                                             
         CLI   8(R2),C'S'                                                       
         BNE   ERRINV                                                           
*                                                                               
         LR    R3,R2                                                            
         LLC   R0,0(R2)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   7(R3),0                  IS THERE SUPERVISOR OUTPUT?             
         BE    ERRINV                                                           
*                                                                               
         MVC   BYRSW,8(R3)                                                      
         OC    BYRSW,SPACES                                                     
         MVI   8(R2),C' '               CLEAR SELECT ENTRY                      
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         MVI   PFKEY,4                  FORCE SUPV PF KEY                       
         BAS   RE,SELBYR                                                        
*                                                                               
CS20     BAS   RE,NXTSCRF               R2 POINTS TO DATA FIELD                 
         LA    R0,SPCENDH                                                       
         CR    R2,R0                    END OF SCREEN?                          
         BE    CSX                      YES,EXIT                                
*                                                                               
CS30     BAS   RE,NXTSCRF               R2 POINTS TO NEXT SELECT FIELD          
         B     CS10                                                             
*                                                                               
CSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO SELECT BUYER RECORD                                                
*                                                                               
SELBYR   NTR1                                                                   
         MVC   MPF04ACT,SPACES                                                  
         CLC   =C'SELECT',CONACT                                                
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
*                                                                               
         GOTO1 INITPFKY,DMCB,MPFTABLE                                           
SLBYX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        GETS PID NAME USING PIDNUM                                             
*---------------------------------------------------------------------          
GETPIDNM NTR1                                                                   
         USING SA0REC,R6                                                        
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(L'SA0KEY),0(R6)                                             
         BNE   GPIDX                                                            
         TM    SA0STAT,X'20'       LOCKED                                       
         BO    GPIDX                                                            
*                                                                               
         USING SAPALD,R6                                                        
         MVI   ELCODE,SAPALELQ     X'C3' - PERSONAL ID ELEM                     
         BAS   RE,GETEL2                                                        
         BNE   GPIDX                                                            
         MVC   PIDNAME,SAPALPID                                                 
GPIDX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATES PID NAME AND GETS PID NUMBER                                 
*---------------------------------------------------------------------          
VALPID   NTR1                                                                   
         USING SAPEREC,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   VPIDXNO                                                          
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BAS   RE,GETEL2                                                        
         BNE   VPIDXNO                                                          
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
VPIDXYES B     XYES                                                             
VPIDXNO  B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* READ SD PROFILE INTO SDPROF                                                   
*-----------------------------------------------------------*                   
RDPROF   NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0SD'                                                 
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SDPROF,DATAMGR                                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RPX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*-----------------------------------------------------------*                   
* FILTER RECORDS BY CLIENT                                                      
* AIO MUST POINT TO SUPERVISOR RECORD                                           
*-----------------------------------------------------------*                   
FLTCLT   NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         USING SPVRECD,R6                                                       
         LA    R6,SPVEL            FIRST ELEMENT                                
*                                                                               
FLTC10   DS    0H                                                               
         CLI   0(R6),X'00'          END OF RECORD?                              
         JE    XNO                 NO CLIENT FOUND IN ANY MEDIA                 
         CLI   0(R6),X'02'         LOWER THAN X'02' (TV)?                       
         BL    FLTC20              GET NEXT ELEMENT                             
         CLI   0(R6),X'05'         HIGHETR THAN X'05' (NETWORK)?                
         JH    XNO                 NO CLIENT FOUND IN ANY MEDIA                 
*                                                                               
         CLC   2(3,R6),FILTCLT     FILTER CLIENT MATCH?                         
         JE    XYES                SUCCESS                                      
FLTC20   DS    0H                  OTHERWISE, GET NEXT ELEMENT                  
         LLC   R0,1(R6)                                                         
         AR    R6,R0               ADVANCE TO NEXT ELEMENT                      
         B     FLTC10                                                           
*                                                                               
FLTCLTX  J     XYES                                                             
         DROP  R6                                                               
*-----------------------------------------------------------*                   
* FILTER RECORDS IF SUPERVISOR IS A PLANNER =Y/N                                
* AIO MUST POINT TO SUPERVISOR RECORD                                           
*-----------------------------------------------------------*                   
FLTPLAN  NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         USING SPVRECD,R6                                                       
         LA    R6,SPVEL            FIRST ELEMENT                                
*                                                                               
FLTP10   CLI   0(R6),0             END OF RECORD?                               
         JE    XNO                 YES                                          
         CLI   0(R6),X'22'         PERSON ID ELEMENT?                           
         BE    FLTP20              YES                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0               ADVANCE TO NEXT ELEMENT                      
         B     FLTP10                                                           
*                                                                               
         USING SPIDELD,R6                                                       
FLTP20   TM    FILTFLAG,FILTPNYQ   PLAN=YES FILTER?                             
         BZ    FLTP25              NOPE                                         
         TM    SPIDFLAG,SPIDFPLN   SUPERVISOR IS A PLANNER?                     
         JZ    XNO                 NOPE                                         
         B     FLTPLANX                                                         
*                                                                               
FLTP25   TM    SPIDFLAG,SPIDFPLN   SUPERVISOR IS A PLANNER?                     
         JO    XNO                 YES, DONT WANT THIS ONE                      
*                                                                               
FLTPLANX J     XYES                                                             
         DROP  R6                                                               
*-----------------------------------------------------------*                   
* VALIDATE OPTIONS IN VR (PLAN=Y/N AND D=1-90)                                  
*-----------------------------------------------------------*                   
VALOPTS  NTR1                                                                   
*                                                                               
         MVI   OPTFLAG,0                   CLEAR OPTIONS FLAG                   
         MVI   DAYS,0                      CLEAR DAYS                           
         LA    R2,SPCOPTH                  OPTION FIELD HEADER                  
         CLI   5(R2),0                     ANY INPUT?                           
         BE    VOPTX                       NO                                   
*                                                                               
         XC    SCANAREA,SCANAREA                                                
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         CLI   DMCB+4,2                 # OF FILTER > 2?                        
         BH    ERRINV                   YES, ERROR                              
*                                                                               
         LA    R6,SCANAREA              PLAN=Y/N & D=1-90 IS ACCEPTED           
*                                                                               
VOPT10   CLI   0(R6),1                  FILTER NAME IS 1 CHAR?                  
         BE    VOPTDAYS                 YES, TEST FOR DAYS FILETR               
         CLI   0(R6),4                  FILTER NAME IS 4 CHARS?                 
         BNE   ERRINV                   NO, ERROR                               
*                                                                               
         CLC   =C'PLAN',12(R6)          FILTER MUST START WITH PLAN             
         BNE   ERRINV                                                           
         CLI   1(R6),1              >   RIGHT SIDE OF FILTER EXPRESSION         
         BNE   ERRINV               >   1                                       
         CLI   22(R6),C'Y'         PLAN=Y?                                      
         BNE   *+12                NO                                           
         OI    OPTFLAG,SPIDFPLN    TURN ON SPIDFLAG X'80'                       
         B     VOPT20              CHECK NEXT FILTER                            
         CLI   22(R6),C'N'         PLAN=N?                                      
         BNE   ERRINV              NO, ERROR                                    
         B     VOPT20              CHECK NEXT FILTER                            
*                                                                               
VOPTDAYS CLI   12(R6),C'D'         DAYS FILTER?                                 
         BNE   ERRINV              NO, ERROR                                    
         TM    3(R6),X'80'         NUMERIC FIELD?                               
         BZ    ERRINV              NO, ERROR                                    
         ICM   R1,15,8(R6)         BINARY VALUE                                 
         BZ    ERRINV              IF ZERO ERROR                                
         C     R1,=F'90'           > 90?                                        
         BH    ERRINV              YES, ERROR                                   
         CLI   DAYS,0              HAVE THIS FILTER ALREADY?                    
         BNE   ERRINV              YES, ERROR                                   
         STC   R1,DAYS             STORE THE DAYS                               
*                                                                               
VOPT20   LA    R6,32(R6)           NEXT OPTION                                  
         CLI   0(R6),0             ANY MORE OPTIONS TO TEST FOR?                
         BNE   VOPT10              YES                                          
*                                                                               
VOPTX    B     XIT                                                              
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         XC    BYRSW,BYRSW                                                      
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    SETUPX                                                           
         CLI   ACTEQU,ACTREP       ACTION REPORT?                               
         BE    SETUPX              YES                                          
*                                                                               
         MVC   MPF02ACT,SPACES                                                  
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF02ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF04ACT,SPACES                                                  
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF06ACT,SPACES                                                  
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF06ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF08ACT,SPACES                                                  
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF08ACT,=CL8'DISPLAY'                                           
*                                                                               
         CLI   CALLSP,0             ANYTHING WAITING TO RETURN TO?              
         BE    *+8                  NO                                          
         NI    SPCREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    SPCREH+6,X'80'       TRANSMIT THE RESULT                         
*                                                                               
SETUP99  GOTO1 INITPFKY,DMCB,MPFTABLE                                           
SETUPX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
MPFTABLE DS    0X                                                               
* PF02 = OFFICE                                                                 
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYGRP '        RECORD: OFFICE                               
MPF02ACT DC    CL8'       '        ACTION:                                      
MPF02    DC    AL1(KEYTYTWA,L'SPCOFF-1),AL2(SPCOFF-T217FFD)                     
MPF02X   EQU   *                                                                
*                                                                               
* PF04 = BUYER                                                                  
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYER  '        RECORD: BUYER                                
MPF04ACT DC    CL8'       '        ACTION:                                      
MPF04    DC    AL1(KEYTYTWA,L'SPCOFF-1),AL2(SPCOFF-T217FFD)                     
         DC    AL1(KEYTYWS,L'BYRSW-1),AL2(BYRSW-SYSSPARE)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYWS,L'MEDELTX-1),AL2(MEDELTX-SYSSPARE)                   
MPF04X   EQU   *                                                                
*                                                                               
* PF06 = TOP                                                                    
         DC    AL1(MPF06X-*,06,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF06ACT DC    CL8' '                                                           
MPF06X   EQU   *                                                                
*                                                                               
* PF08 = DOWN                                                                   
         DC    AL1(MPF08X-*,08,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF08ACT DC    CL8' '                                                           
MPF08X   EQU   *                                                                
*                                                                               
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
PFILTERS DS    0H                                                               
PBUYGRP  DS    CL2                                                              
PSPVINTL DS    CL4                                                              
POFFICE  DS    CL2                                                              
PRPTBY   DS    CL1                                                              
PMEDIA   DS    CL1                                                              
PPLANNER DS    CL1                                                              
PFILTLEN EQU   *-PFILTERS                                                       
*                                                                               
DASHES   DC    25C'-'                                                           
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'02'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    X'FF'                                                            
MEDTBLEN EQU   *-MEDTAB                                                         
*                                                                               
CLHEAD   DC    CL79'     CODE CLIENT NAME           FLT           CODE +        
               CLIENT NAME           FLT'                                       
CGHEAD   DC    CL79'     ID CODE NAME               FLT           ID CO+        
               DE NAME               FLT'                                       
BUHEAD   DC    CL79'SEL  CODE BUYER NAME                     SEL  CODE +        
               BUYER NAME'                                                      
MKHEAD   DC    CL79'     MKT  MARKET NAME           BYR           MKT  +        
               MARKET NAME           BYR'                                       
*                                                                               
VKRPT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PFILTERS,PFILTERS                                                
***                                                                             
* SET BUYGRP FILTER                                                             
***                                                                             
         LA    R2,SVRBGPH           BUYER GROUP CODE                            
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VK01                 NO                                          
         CLI   8(R2),C' '           FIRST CHAR MUST BE > BLANK                  
         BNH   ERRINV                                                           
         MVC   PBUYGRP,8(R2)                                                    
         OC    PBUYGRP,SPACES                                                   
***                                                                             
* SET SUPERVISOR INITIALS FILTER                                                
***                                                                             
VK01     LA    R2,SVRINTH           VALIDATE SUPERVISOR CODE                    
         CLI   5(R2),0              SUPV INPUT?                                 
         BE    VK02                 NO                                          
         CLI   8(R2),C' '           FIRST CHAR MUST BE > BLANK                  
         BNH   ERRINV                                                           
         CLI   5(R2),2              SUPV CODE LEN. AT LEAST 2                   
         BL    ERRINV                                                           
         MVC   PSPVINTL,8(R2)                                                   
         OC    PSPVINTL,SPACES                                                  
***                                                                             
* SET OFFICE FILTER                                                             
***                                                                             
VK02     LA    R2,SVROFFH           OFFICE CODE                                 
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VK03                 NO                                          
         MVC   POFFICE,8(R2)                                                    
***                                                                             
* REPORT BY CLIENT OR BUYER?                                                    
***                                                                             
VK03     LA    R2,SVRLISH           LIST BY CLIENT OR BUYER?                    
         CLI   5(R2),0              ANY INPUT?                                  
         BE    ERRMIS               NO, INPUT REQUIRED                          
         MVC   PRPTBY,8(R2)                                                     
         CLI   8(R2),C'C'           REPORT BY CLIENT?                           
         BE    VK04                 YES                                         
         CLI   8(R2),C'B'           REPORT BY BUYER?                            
         BNE   ERRINV               NO, ERROR                                   
***                                                                             
* SET MEDIA FILTER                                                              
***                                                                             
VK04     MVC   RPTMEDTB(MEDTBLEN-1),MEDTAB    SAVE MEDIA TABLE VALUES           
         MVC   RPTMEDTB+MEDTBLEN-1(MEDTBLEN),MEDTAB                             
         OI    RPTMEDTB+MEDTBLEN,X'10'                                          
         OI    RPTMEDTB+MEDTBLEN+2,X'10'                                        
         OI    RPTMEDTB+MEDTBLEN+4,X'10'                                        
*                                                                               
         LA    R2,SVRMEDH           MEDIA FIELD                                 
         CLI   5(R2),0              ANY INPUT?                                  
         BH    *+14                 NO                                          
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'           DON'T WANT/NEED MEDIA NIBBLE                
         CLI   SVRMEDH+5,0                                                      
         BE    VK06                                                             
         MVC   PMEDIA,QMED          1 CHAR MEDIA FILTER                         
         LA    R2,RPTMEDTB          MEDIA TABLE FOR REPORTING BY CLIENT         
*                                                                               
VK05     CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    VK06                 YES                                         
         CLC   QMED,0(R2)           FILTERING ON THIS MEDIA?                    
         BE    *+10                 YES - DON'T CLEAR THIS TABLE ENTRY          
         XC    0(2,R2),0(R2)        NO - CLEAR THIS TABLE ENTRY                 
         LA    R2,2(R2)             BUMP TO NEXT MEDIA ENTRY                    
         B     VK05                                                             
***                                                                             
* SET PLANNER FILTER                                                            
***                                                                             
VK06     CLI   PRPTBY,C'B'          REPORT BY BUYER?                            
         BE    VKRX                 NO, DON'T CARE ABOUT PLANNER                
         LA    R2,SVRPLNH                                                       
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VKRX                 NO                                          
         MVC   PPLANNER,8(R2)                                                   
         CLI   8(R2),C'Y'           PLANNER=Y?                                  
         BE    VKRX                 YES                                         
         CLI   8(R2),C'N'           PLANNER=N?                                  
         BNE   ERRINV               NO, ERROR                                   
*                                                                               
VKRX     XIT1                                                                   
*                                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R1,=A(HEDSPECS)      SET UP HEADHOOK AND SPECS                   
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         ST    R1,HEADHOOK                                                      
         MVC   SECALPHA,AGENCY      SAVE SECURITY AGENCY                        
***                                                                             
* READ THE SUPERVISOR RECORD                                                    
***                                                                             
         LA    R6,KEY               BUILD SUPERVISOR KEY                        
         USING SPVRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,PBUYGRP                                                  
         MVC   SPVKSPV,PSPVINTL                                                 
         MVC   AIO,AIO1             MAKE SURE USING CORRECT AIO                 
         GOTO1 HIGH                                                             
         B     PR50                                                             
*                                                                               
PR45     GOTO1 SEQ                                                              
*                                                                               
PR50     LA    R6,KEY               BUILD SUPERVISOR KEY                        
         CLC   KEY(3),KEYSAVE       HAVE SUPERVISOR REC FOR THIS AGY?           
         BNE   PRXIT                NO, DONE                                    
         OC    PBUYGRP,PBUYGRP      ANY BUYER GROUP FILTER?                     
         BZ    *+14                 NO                                          
         CLC   SPVKOFC,PBUYGRP      MATCH ON BUYER GROUP?                       
         BNE   PR45                 NO, READ SEQ                                
         OC    PSPVINTL,PSPVINTL    ANY SUPERVISOR INITIAL FILTER?              
         BZ    *+14                 NO                                          
         CLC   SPVKSPV,PSPVINTL     MATCH ON SUPERVISOR INITIAL?                
         BNE   PR45                 NO, READ READ SEQ                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
         LA    R4,P                                                             
         USING PLINE1,R4                                                        
         MVC   PLBGRP,SPVKOFC       BUYER GROUP                                 
         MVC   PLBCODE,SPVKSPV      SUPERVISOR INITIALS                         
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SPVNAMED,R6                                                      
         MVC   PLFNAME,SPVFNAME     FIRST NAME                                  
         MVC   PLLNAME,SPVLNAME     LAST NAME                                   
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'         GET PID                                     
         BAS   RE,GETEL                                                         
         BNE   PR52                                                             
         USING SPIDELD,R6                                                       
         MVC   PIDNUM,SPIDNO        BINARY PID                                  
         BRAS  RE,GETPIDNM          GET THE PID NAME                            
         MVC   PLPID,PIDNAME        MOVE IT TO THE PRINT LINE                   
*                                                                               
PR52     CLI   PRPTBY,C'C'          REPORT BY CLIENT?                           
         BE    PR55                 YES                                         
***                                                                             
* READ THE BUYER RECORD                                                         
***                                                                             
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BYRRECD,R6                                                       
         MVC   BYRPTYP2,=X'0DE3'                                                
         MVC   BYRPAGY2,SAVEKEY+2                                               
         MVC   BYRPOFC2,SAVEKEY+3                                               
         MVC   BYRPSPV2,SAVEKEY+5                                               
         GOTO1 HIGH                                                             
         B     PR52B                                                            
*                                                                               
PR52A    GOTO1 SEQ                                                              
*                                                                               
PR52B    CLC   KEY(9),KEYSAVE       HAVE CORRECT BUYER KEY?                     
         BNE   PR53                 NO, DONE WITH THIS BUYER                    
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         USING BYRNAMED,R6                                                      
         OC    POFFICE,POFFICE      ANY OFFICE FILTER?                          
         BZ    *+14                 NO                                          
         CLC   POFFICE,BYROFF       YES - DOES OFFICE CODE MATCH?               
         BNE   PR52A                NO, FILTER THIS RECORD OUT                  
         MVC   PLOFFICE,BYROFF                                                  
*                                                                               
         MVC   PLBUYER,KEY+(BYRPBYR2-BYRPTYP2)                                  
         MVC   PLBNAME(L'BYRLNAME),BYRLNAME                                     
         MVC   PLBNAME+15(L'BYRFNAME),BYRFNAME                                  
         GOTO1 SQUASHER,DMCB,PLBNAME,(C',',L'PLBNAME)                           
         TM    BYROPT1,BYROPT1_ASS  ASSISTANT?                                  
         BZ    *+8                  NO                                          
         MVI   PLASSTNT,C'Y'        YES - FLAG ASSISTANT                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR52A                                                            
*                                                                               
PR53     MVC   KEY,SAVEKEY          RE-READ THE SUPERVISOR RECORD               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      HAVE SAME REC WE JUST PROCESSED?            
         BE    PR45                 YES                                         
         DC    H'0'                 NO - DEATH                                  
***                                                                             
* REPORT BY CLIENT                                                              
***                                                                             
PR55     L     R6,AIO                                                           
         MVI   ELCODE,SPIDELQ       ID ELEMENT                                  
         BAS   RE,GETEL             HAVE ONE?                                   
         BNE   PR55B                NO, NOT REQUIRED                            
*                                                                               
         USING SPIDELD,R6                                                       
         TM    SPIDFLAG,SPIDFPLN    SUPERVISOR IS A PLANNER?                    
         BZ    PR55A                NO                                          
         CLI   PPLANNER,C'N'        REPORT BY NON-PLANNER ONLY?                 
         BE    PR45                 YES, THIS RECORD GETS FILTERED OUT          
         MVI   PLPLAN,C'Y'                                                      
         B     PR55B                                                            
*                                                                               
PR55A    CLI   PPLANNER,C'Y'        REPORT BY PLANNER ONLY?                     
         BE    PR45                 YES - THIS SUPERVISOR ISNT PLANNER          
*                                                                               
PR55B    LA    R2,RPTMEDTB          MEDIA TABLE                                 
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
*                                                                               
PR60     CLI   0(R2),X'FF'          END OF MEDIA TABLE?                         
         BE    PR65                 YES                                         
         CLI   0(R2),0              PROCESSING THIS MED IN THIS REPORT?         
         BE    PR61                 NO                                          
         MVC   ELCODE,1(R2)         LOOK FOR THIS MEDIA ELEMENT                 
         MVC   PLMEDIA,0(R2)        MOVE MEDIA TO PRINT LINE                    
*                                                                               
PR60A    BAS   RE,NEXTEL            FOUND THIS MEDIA ELEMENT?                   
         BNE   PR61                 NO                                          
         USING SPVCLTD,R6                                                       
         MVC   PLFILTER,SPVFILT     MOVE FILTER TO THE PRINT LINE               
         OI    PLFILTER,X'40'       SPACE PAD FILTER IN CASE ITS NULL           
         MVC   PLCLT,SPVCLT         MOVE CLIENT CODE TO THE PRINT LINE          
*                                                                               
         TM    1(R2),X'10'          CLIENT GROUP?                               
         BNZ   PR60B                YES                                         
*                                                                               
         XC    KEY,KEY              READ THE CLIENT RECORD                      
         MVC   KEY+1(1),BAGYMD      A/M                                         
         NI    KEY+1,X'F0'          TURN OFF MEDIA BIT                          
         CLI   0(R2),C'T'           MEDIA T?                                    
         BNE   *+8                  NO                                          
         OI    KEY+1,X'01'          YES - MEDIA T = X'01'                       
         CLI   0(R2),C'R'           MEDIA R?                                    
         BNE   *+8                  NO                                          
         OI    KEY+1,X'02'          YES - MEDIA R = X'02'                       
         CLI   0(R2),C'X'           MEDIA X?                                    
         BNE   *+8                  NO                                          
         OI    KEY+1,X'04'          YES - MEDIA X = X'04'                       
*                                                                               
         GOTO1 CLPACK,DMCB,SPVCLT,KEY+2                                         
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      HAVE THIS CLIENT RECORD?                    
         BE    *+6                  YES                                         
         DC    H'0'                 NO - WHAT THE DIDLEY!?                      
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   PLCLTNAM,CNAME-CLTHDR(R3)                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR60A                                                            
         DROP  R6                                                               
*                                                                               
         USING SPVCGRD,R6                                                       
PR60B    L     R1,=A(SPCGRTAB)                                                  
         A     R1,RELO                                                          
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
PR60C    CLC   SPVCGRID,2(R1)                                                   
         BE    PR60D                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,PR60C                                                         
         DC    H'0'                                                             
*                                                                               
PR60D    MVC   PLCLT(2),0(R1)                                                   
         MVC   PLFILTER,SPVCGRF     MOVE FILTER TO THE PRINT LINE               
         OI    PLFILTER,X'40'       SPACE PAD FILTER IN CASE ITS NULL           
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,SPVCGRP,PLCLT+2,2,=C'TOG'                              
         DROP  RF                                                               
*                                                                               
         LR    R5,R6                ELEM DISPLACEMENT IN SUPV REC               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPKEY,R3                                                        
         MVI   GRPKTYP,GRPKTYPQ         X'0D'                                   
         MVI   GRPKSTYP,GRPKCTYQ        X'04'                                   
         MVC   GRPKAGMD,BAGYMD          A/M                                     
         MVC   GRPKID,SPVCGRID          CLIENT GROUP ID                         
         MVC   GRPKCODE,SPVCGRP         CLIENT GROUP CODE                       
         DROP  R3,R6                                                            
*                                                                               
         MVC   AIO,AIO2             DONT CLOBBER SUPV RECORD                    
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVC   PLCLTNAM+3(5),=C'?????'                                          
         MVC   BYTE,ELCODE          SAVE ELEMENT CODE FROM SUPV REC             
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         MVC   ELCODE,BYTE          RESTORE ELEMENT CODE                        
         BNE   PR60E                                                            
         OC    GRPGNAM1,GRPGNAM1        DONT MOVE IN BLANKS                     
         BZ    PR60E                                                            
         MVC   PLCLTNAM+3(17),GRPGNAM1                                          
         OC    GRPGNAM2,GRPGNAM2        DONT MOVE IN BLANKS                     
         BZ    PR60E                                                            
         CLC   GRPGNAM1,GRPGNAM2        NAME1 < NAME2                           
         BL    PR60E                    YES                                     
         MVC   PLCLTNAM+3(17),GRPGNAM2                                          
         DROP  R6                                                               
*                                                                               
* READ GROUP ID DEFINITION RECORD TO DISPLAY ACCORDING TO BREAK LENGTHS         
*                                                                               
PR60E    LA    R3,KEY                                                           
         USING GRPKEY,R3                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(GRPKCODE-GRPKEY),KEYSAVE                                     
         DROP  R3                                                               
*                                                                               
         MVC   AIO,AIO2             DONT CLOBBER SUPV RECORD                    
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   BYTE,ELCODE          SAVE ELEMENT CODE FROM SUPV REC             
         MVI   ELCODE,GRPBRKCQ      BREAK DESCRIPTION                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING GRPBRKD,R6                                                       
         MVC   ELCODE,BYTE          RESTORE ELEMENT CODE                        
         LLC   R3,GRPBK1LN          BREAK 1 LENGTH                              
         LLC   R1,GRPBK2LN          BREAK 2 LENGTH                              
         AR    R3,R1                ADD THEM                                    
         LA    R1,PLCLT+5           4TH DIGIT OF GROUP CODE                     
         DROP  R6                                                               
*                                                                               
PR60F    CHI   R3,4                 DISPLAYED CORRECT LENGTH?                   
         BE    PR60G                YES                                         
         MVI   0(R1),X'40'          CLEAR EXTRA DIGIT                           
         BCTR  R1,0                 POINT TO PREVIOUS DIGIT                     
         AHI   R3,1                 UNTIL BREAK LENGTH IS 4                     
         B     PR60F                                                            
*                                                                               
PR60G    GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    R6,R5                ELEM DISPLACEMENT IN SUPV REC               
         B     PR60A                                                            
*                                                                               
PR61     LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MEDIA TABLE           
         L     R6,AIO1              RE-POINT TO START OF RECORD                 
         LA    R6,24(R6)            FIRST ELEMENT                               
         B     PR60                                                             
*                                                                               
PR65     MVI   PLMEDIA,X'40'        ALREADY PRINTED MEDIA IF FOUND              
         GOTO1 SPOOL,DMCB,(R8)      PRINT LINE IN CASE OF NO CLIENTS            
         L     R6,AIO1              SUPERVISOR RECORD BUFFERED HERE             
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)        RE-READ THE SUPERVISOR RECORD               
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      HAVE THE SUPERVISOR RECORD?                 
         BE    PR45                 YES                                         
         DC    H'0'                 NO - WHAT THE DIDLEY!?                      
*                                                                               
PRXIT    XIT1                                                                   
***********************************************************************         
*        CLEAR BOTTOM SCREEN - CLEARS LIST DISPLAY - NOT SELECT FIELDS          
***********************************************************************         
*                                                                               
CLBTSC   NTR1  BASE=*,LABEL=*      CLEARS SCREEN FROM ADD CLI DOWN              
         LA    R2,SPCSELH        1ST LINE                                       
         LA    R3,SPCPFKYH                                                      
*                                                                               
CLBTSC10 TM    1(R2),X'20'              IS FIELD PROTECTED?                     
         BO    CLBTSC15                 PROTECTED, CLEAR                        
         TM    6(R2),X'20'              IS FIELD MARKED FOR PROT?               
         BNO   CLBTSC20                 NO, CHECK NEXT                          
CLBTSC15 LLC   R1,0(R2)            FIELD LENGTH                                 
         SHI   R1,9                8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
CLBTSC20 OI    6(R2),X'80'         TRANSMIT                                     
****** MAKES SURE APPROPRIATE FIELDS ARE TRANSMITTED AS UNPROTECTED             
*                                                                               
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLBTSC10            NO                                           
CBSX     J     XIT                                                              
         EJECT                                                                  
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,H6                                                            
         USING PLINE1,R4                                                        
         MVC   PLBGRP,=C'Buy'                                                   
         MVC   PLBCODE+1(2),=C'ID'                                              
         MVC   PLFNAME(5),=C'First'                                             
         MVC   PLLNAME(4),=C'Last'                                              
         MVC   PLPID(3),=C'PID'                                                 
         CLI   PRPTBY,C'B'          REPORT BY BUYER?                            
         BE    HK10                 YES                                         
         MVC   PLPLAN(2),=C'Pl'                                                 
         MVI   PLMEDIA,C'M'                                                     
         MVC   PLFILTER(3),=C'Flt'                                              
         MVC   PLCLT,=C'Clt'                                                    
         MVC   PLCLTNAM+4(11),=C'Client name'                                   
         B     HK20                                                             
*                                                                               
HK10     MVC   PLOFFICE,=C'Off'                                                 
         MVC   PLBUYER(5),=C'Buyer'                                             
         MVC   PLBNAME+7(10),=C'Buyer name'                                     
         MVC   PLASSTNT(2),=C'As'                                               
*                                                                               
HK20     LA    R4,H7                                                            
         MVC   PLBGRP,=C'Grp'                                                   
         MVC   PLFNAME(4),=C'Name'                                              
         MVC   PLLNAME(4),=C'Name'                                              
         CLI   PRPTBY,C'B'          REPORT BY BUYER?                            
         BE    *+14                 YES                                         
         MVC   PLPLAN(2),=C'an'                                                 
         B     *+10                                                             
         MVC   PLASSTNT(2),=C'st'                                               
*                                                                               
         LA    R4,H8                                                            
         MVC   PLBGRP,DASHES                                                    
         MVC   PLBCODE,DASHES                                                   
         MVC   PLFNAME,DASHES                                                   
         MVC   PLLNAME,DASHES                                                   
         MVC   PLPID,DASHES                                                     
         CLI   PRPTBY,C'B'          REPORT BY BUYER?                            
         BE    HK30                 YES                                         
         MVC   PLPLAN(2),DASHES                                                 
         MVI   PLMEDIA,C'-'                                                     
         MVC   PLFILTER(3),DASHES                                               
         MVC   PLCLT,DASHES                                                     
         MVC   PLCLTNAM,DASHES                                                  
         B     HKXIT                                                            
*                                                                               
HK30     MVC   PLOFFICE,DASHES                                                  
         MVC   PLBUYER(5),DASHES                                                
         MVC   PLBNAME,DASHES                                                   
         MVC   PLASSTNT(2),DASHES                                               
*                                                                               
HKXIT    XIT1                                                                   
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,2,REQUESTOR                                                   
         SSPEC H1,52,C'SUPERVISOR REPORT'                                       
         SSPEC H1,92,AGYNAME                                                    
*                                                                               
         SSPEC H2,2,PAGE                                                        
         SSPEC H2,52,C'-----------------'                                       
         SSPEC H2,92,AGYADD                                                     
*                                                                               
         SSPEC H3,92,REPORT                                                     
         SSPEC H3,104,RUN                                                       
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPCGRTAB                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFME7D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF7D          LIST SCREEN                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM5FD          REPORT SCREEN                                
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENGRP          CLIENT GROUP DSECT                           
         EJECT                                                                  
       ++INCLUDE SPGENOFC                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBYR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
OFFICE   DS    CL2                      STORAGE FOR INPUT OFFICE                
SUPV     DS    CL4                      STORAGE FOR INPUT SUPERVISOR            
BYRSW    DS    CL4                                                              
STARTMKT DS    XL2                      3 MARKET FIELDS FOR PAGING              
LASTMKT  DS    XL2                                                              
FRSTMKT  DS    XL2                                                              
STARTCLT DS    CL3                      3 CLIENT FIELDS FOR PAGING              
LASTCLT  DS    CL3                                                              
FRSTCLT  DS    CL3                                                              
STARTBYR DS    CL4                      3 BUYER FIELDS FOR PAGING               
LASTBYR  DS    CL4                                                              
FRSTBYR  DS    CL4                                                              
BYRTAB   DS    CL252                    BUYER TABLE(63)                         
BYTBEND  DS    0H                       END TABLE FLAG                          
SAVEKEY  DS    CL13                     SAVED KEYS                              
SAVEKEY2 DS    CL13                                                             
LASTKEY  DS    CL13                                                             
MEDELCD  DS    XL1                      ELCODE FOR A PARTICULAR MEDIA           
MEDELTX  DS    CL1                      ELCODE FOR A PARTICULAR MEDIA           
CLTCODE  DS    CL3                      INPUT ADD CLIENT FIELD CODE             
CLTFILT  DS    CL1                      CLIENT FILTER                           
TEMPFLD  DS    XL12                     FOR VALIMED,MKT,CLT CALLS               
AGYX     DS    XL1                      AGENCY HEX, MEDIA=0                     
STATUS   DS    XL1                      STATUS BYTE FLAG                        
KEYCHG   EQU   X'80'                    KEY HAS CHANGED                         
ERRNUM   DS    XL2                      FOR ERROR CODES                         
         DS    0H                       FORCE ALIGN FOR CLIENT STORAGE          
*                                                                               
ADDCLTS  DS    8CL3                     STORAGE FOR ADDED CLIENTS               
ADCLEND  DS    0H                       END TABLE FLAG                          
ADCLLNEQ EQU   ADCLEND-ADDCLTS          LENGTH OF TABLE                         
*                                                                               
DELCLTS  DS    8CL3                     STORAGE FOR DELETED CLIENTS             
DECLEND  DS    0H                                                               
DECLLNEQ EQU   DECLEND-DELCLTS                                                  
*                                                                               
ADDCGRP  DS    8CL3                     STORAGE FOR ADDED CGROUPS               
ADCGEND  DS    0H                       END TABLE FLAG                          
ADCGLNEQ EQU   ADCGEND-ADDCGRP          LENGTH OF TABLE                         
*                                                                               
DELCGRP  DS    8CL3                     STORAGE FOR DELETED CGROUPS             
DECGEND  DS    0H                       END TABLE FLAG                          
DECGLNEQ EQU   DECGEND-DELCGRP          LENGTH OF TABLE                         
*                                                                               
DISKADD  DS    XL4                      SAVE D/A                                
SAVESEL  DS    CL1                      STORAGE FOR THISLSEL                    
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                                                              
KEY2     DS    CL50                                                             
SDPROF   DS    XL16                                                             
SCANAREA DS    CL256                    AREA USED FOR SCANNER                   
*                                                                               
FILTMAXQ EQU   2                                                                
FILTFLAG DS    XL1                                                              
FILTCLTQ EQU   X'80'                    CLIENT FILTER                           
FILTPNYQ EQU   X'40'                    PLAN = YES                              
FILTPNNQ EQU   X'20'                    PLAN = NO                               
*                                                                               
OPTFLAG  DS    XL1                                                              
*                                                                               
FILTCLT  DS    CL3                                                              
CGROUP   DS    0CL3                                                             
CGROUPID DS    CL1                                                              
CGROUPCD DS    CL2                                                              
CGELCODE DS    CL1                                                              
CGROUPF  DS    CL1                                                              
*                                                                               
DAYS     DS    CL1                                                              
*                                                                               
RPTMEDTB DS    CL(MEDTBLEN*2-1)                                                 
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
**** ONLINE LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSOFFCD  DS    CL2                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL5                                                              
LSSUPCD  DS    CL4                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL2                                                              
LSSUPNM  DS    CL26                                                             
         EJECT                                                                  
*                                                                               
LISD     DSECT                          DSECT TO LIST SUPV DATA                 
*                                                                               
* LIST BY CLIENT                                                                
LSSELH   DS    CL8                      SELECT FIELD HEADER                     
         DS    CL1                      SELECT FIELD                            
LSDSLQ   EQU   *-LSSELH                 LEN EQUATE OF SPCSEL                    
         DS    CL8                      OUTPUT HEADER                           
CLC      DS    CL3                      CLIENT CODE                             
         DS    CL2                                                              
CLNM     DS    CL20                     CLIENT NAME                             
         DS    CL2                                                              
CLFLT    DS    CL1                      CLIENT FILTER                           
         DS    CL5                                                              
LSDLQ    EQU   *-LSSELH                 LIST DSECT LEN EQUATE                   
*                                                                               
         EJECT                                                                  
*                                                                               
* LIST BY CLIENT GROUP                                                          
         ORG   LISD                                                             
         DS    CL8                      SELECT FIELD HEADER                     
         DS    CL1                      SELECT FIELD                            
         DS    CL8                      OUTPUT HEADER                           
CGID     DS    CL2                      CLIENT GROUP ID                         
         DS    CL1                                                              
CGCD     DS    CL4                      CLIENT GROUP CODE                       
         DS    CL1                                                              
CGNAME   DS    CL22                     CLIENT GROUP NAME                       
         DS    CL1                                                              
CGFLT    DS    CL1                      CLIENT GROUP FILTER                     
         DS    CL5                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* LIST BY BUYER                                                                 
         ORG   LISD                                                             
         DS    CL8                      SELECT HEADER                           
         DS    CL1                      SELECT FIELD                            
         DS    CL8                      OUTPUT HEADER                           
BYRC     DS    CL4                      BUYER CODE                              
         DS    CL1                                                              
BYNM     DS    CL25                     BUYER NAME                              
         DS    CL3                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* LIST BY MARKET                                                                
         ORG   LISD                                                             
         DS    CL8                      SELECT HEADER                           
         DS    CL1                      SELECT FIELD                            
         DS    CL8                      OUTPUT HEADER                           
MKTC     DS    CL4                      BUYER CODE                              
         DS    CL1                                                              
MKNM     DS    CL20                     BUYER NAME                              
         DS    CL2                                                              
MKBYCD   DS    CL4                      BUYER CODE                              
         DS    CL2                                                              
         SPACE 3                                                                
PLINED   DSECT                                                                  
PLINE1   DS    0X                                                               
PLBGRP   DS    CL2                                                              
         DS    CL1                                                              
         DS    CL1                                                              
PLBCODE  DS    CL4                                                              
         DS    CL1                                                              
PLFNAME  DS    CL10                                                             
         DS    CL1                                                              
PLLNAME  DS    CL14                                                             
         DS    CL1                                                              
PLPID    DS    CL8                                                              
         DS    CL1                                                              
PLPLAN   DS    CL1                                                              
         DS    CL2                                                              
PLMEDIA  DS    CL1                                                              
         DS    CL1                                                              
PLFILTER DS    CL1                                                              
         DS    CL3                                                              
PLCLT    DS    CL3                                                              
         DS    CL1                                                              
PLCLTNAM DS    CL20                                                             
         DS    CL1                                                              
         ORG   PLPLAN                                                           
PLOFFICE DS    CL3                                                              
         DS    CL1                                                              
PLBUYER  DS    CL4                                                              
         DS    CL2                                                              
PLBNAME  DS    CL25                                                             
         DS    CL1                                                              
PLASSTNT DS    CL1                                                              
         DS    CL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPSFM4B   10/07/08'                                      
         END                                                                    
