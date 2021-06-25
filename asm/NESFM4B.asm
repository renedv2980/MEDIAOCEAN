*          DATA SET NESFM4B    AT LEVEL 080 AS OF 10/31/05                      
****************************                                                    
*PHASE T31C4BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C4B  -- SUPERVISOR RECORD MAINTENANCE             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SUPERVISOR RECORDS ON SPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS NESFMA2, AND NESFMA3                         *         
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
         TITLE 'T31C4B - SUPERVISOR RECORD MAINTENANCE'                         
T31C4B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C4B**,R7,RR=R3                                              
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
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         CLI   ACTNUM,ACTREST           ACTION RESTORE?                         
         BE    ERRACT                   INVALID ACTION                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
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
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVC   AIO,AIO1                 MAKE SURE USING CORRECT AIO             
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
         CLI   ACTNUM,ACTLIST           ACTION LIST?                            
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
         CLI   ACTNUM,ACTADD                                                    
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
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
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
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         XC    ADDCLTS(ADCLLNEQ),ADDCLTS                                        
         XC    DELCLTS(DECLLNEQ),DELCLTS                                        
         CLI   ACTNUM,ACTADD                                                    
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
*        VALIDATE PERSONAL ID                                        *          
*                                                                    *          
         LA    R2,SPCPIDH                                                       
         CLI   5(R2),0             REQ'D                                        
         BE    ERRMIS                                                           
         MVC   PIDNAME,SPCPID                                                   
         OC    PIDNAME,SPACES                                                   
         BAS   RE,VALPID                                                        
         BNE   ERRINV                                                           
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
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
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
         LA    R3,ADDCLTS               STORAGE TABLE OF ADDED CLIENTS          
         LA    R5,DELCLTS               STORAGE TABLE OF DEL'D CLIENTS          
*                                                                               
VR45     CLI   5(R2),0                  MEDIA INPUT, CLIENT INPUT?              
         BE    VRX                      NO CLIENT, JUST ADD REC                 
*                                                                               
         CLI   ACTNUM,ACTADD            ACTION ADD?                             
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
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL   STAY HERE FOR ONE TRANSACTION                
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTSEL       CAME FROM LIST?                              
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VRX10                                                            
         XC    KEY,KEY                                                          
*                                                                               
         L     RF,AIO                                                           
         MVC   KEY(13),0(RF)                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         L     RF,AIO                                                           
         CLI   0(RF),X'06'         AGY RECORD?                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 PUTREC                                                           
         B     VRXIT                                                            
*                                                                               
VRX10    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRXIT                                                            
*                                                                               
         L     RF,AIO                                                           
         CLI   0(RF),X'06'         AGY RECORD?                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
VRXIT    B     DR                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         BAS   RE,CLTPSC                CLEAR TOP PORTION OF SCREEN             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'01'        SUPERVISOR NAME ELEMENT                      
         BAS   RE,GETEL                                                         
*                                                                               
         USING SPVNAMED,R6                                                      
         MVC   SPCFNM,SPVFNAME      DISPLAY SUPERVISOR NAME                     
         OI    SPCFNMH+6,X'80'                                                  
         MVC   SPCLNM,SPVLNAME      DISPLAY SUPERVISOR NAME                     
         OI    SPCLNMH+6,X'80'                                                  
*                                                                               
*        DISPLAY PID NAME                                                       
*                                                                               
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
DR3      DS    0H                                                               
*                                                                               
**** "LIST BY" LOGIC                                                            
         LA    R2,SPCLISH                                                       
*                                                                               
         CLI   ACTNUM,ACTSEL            LIST?                                   
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
         BAS   RE,CLBTSC                                                        
*                                                                               
         LA    R2,SPCMEDH                                                       
         CLI   ACTNUM,ACTCHA            ACTION CHANGE?                          
         BE    *+12                     YES, DISPLAY FROM FIRST CLIENT          
         TM    4(R2),X'80'              MEDIA INPUT THIS TIME?                  
         BNO   DR22                     NO, DISPLAY FROM LAST POS               
         XC    STARTCLT,STARTCLT        YES, DISPLAY FROM FIRST CLIENT          
         XC    FRSTCLT,FRSTCLT                                                  
         XC    LASTCLT,LASTCLT                                                  
*                                                                               
DR22     CLI   5(R2),0                  MEDIA INPUT?                            
         BNE   DR25                     YES, CONTINUE                           
         MVI   8(R2),C'N'               NO                                      
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
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1                                                         
*                                                                               
* PFKEY STUFF                                                                   
         CLI   PFAID,8                  DOWN?                                   
         BNE   DR36                     NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTCLT,FRSTCLT          YES,DON'T PAGE DOWN                     
         MVC   STARTCLT,LASTCLT                                                 
         B     DR37                                                             
*                                                                               
DR36     CLI   PFAID,6                  TOP?                                    
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
*                                                                               
         LA    R4,KEY                   USE PAS KEY TO SUPV BY CLIENT           
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
         BNE   *+12                                                             
         MVI   ELCODE,X'04'                                                     
         B     DR46                                                             
         CLI   SPCMED,C'N'                                                      
         BNE   DR46                                                             
         MVI   ELCODE,X'05'                                                     
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
         MVC   TEMPFLD(8),=X'0B00000000030000'                                  
         MVC   TEMPFLD+8(3),CLC         PUT ONSCREEN CLIENT IN FIELD            
         LA    R2,TEMPFLD                                                       
         MVC   AIO,AIO3                                                         
*        MVI   USEIONUM,3               PRESERVE AIO AND AIO2                   
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
         BAS   RE,CLBTSC                CLEAR BOTTOM PORTION OF SCREEN          
*                                                                               
         MVC   SPCSTRT,BUHEAD           SHOW BUYER HEADER                       
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
         CLI   PFAID,8                  DOWN?                                   
         BNE   DR55                     NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTBYR,FRSTBYR          YES,DON'T PAGE DOWN                     
         MVC   STARTBYR,LASTBYR                                                 
         B     DR60                                                             
*                                                                               
DR55     CLI   PFAID,6                  TOP?                                    
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
         BAS   RE,CLBTSC                                                        
         MVC   SPCSTRT,MKHEAD           SHOW MARKET HEADER                      
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
         MVI   8(R2),C'N'               NO                                      
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
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1                                                         
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
         CLI   PFAID,8                  DOWN?                                   
         BNE   DR92A                    NO CHECK FOR PF6= TOP                   
         CLI   SPCENDH+7,0              LAST FIELD BLANK?                       
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTMKT,FRSTMKT          YES,DON'T PAGE DOWN                     
         MVC   STARTMKT,LASTMKT                                                 
         B     DR92B                                                            
*                                                                               
DR92A    CLI   PFAID,6                  TOP?                                    
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
         MVC   AIO,AIO2                                                         
*        MVI   USEIONUM,2                                                       
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
DR100    CLI   ACTNUM,ACTSEL            ACTION SELECT?                          
         BNE   DR100A                   NO, CHECK FOR ACTION CHANGE             
         CLI   SAVESEL,C'C'             YES,DON'T EXIT YET                      
         BE    DR101                                                            
         CLI   SAVESEL,C'D'             DELETE ON FROM LIST?                    
         BE    DR101                                                            
DR100A   CLI   ACTNUM,ACTCHA                                                    
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
DKX      CLI   ACTNUM,ACTSEL                                                    
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
         CLI   ACTNUM,ACTREST           ACTION RESTORE?                         
         BE    XR7A                     YES, ADD BACK P-KEYS                    
         CLI   SAVESEL,C'D'             IS SELECT = DELETE?                     
         BE    XR3                      YES, DELETE P-KEYS                      
         CLI   ACTNUM,ACTDEL            ACTION DELETE?                          
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
XR7A     DS    0H                                                               
         BAS   RE,RCRST                                                         
*                                                                               
         XC    KEY,KEY                  ADD CLT P-KEYS                          
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'        SEARCHING FOR ALL CLIENT -               
         MVC   SPVPAM,AGYX            - P-KEYS TO THE SUPV FOR -                
         MVC   SPVPOFC,OFFICE          - ALL MEDIA                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ DELETED RECORDS                     
         GOTO1 HIGH                                                             
         B     XR8A                                                             
*                                                                               
XR8      GOTO1 SEQ                                                              
XR8A     CLC   KEY(2),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XRX                   NO, DONE                                   
*                                                                               
         CLC   SPVPOFC,OFFICE           CORRECT OFFICE?                         
         BH    XRX                      HIGHER, EXIT                            
         CLC   SPVPSPV,SUPV            CORRECT SUPV?                            
         BNE   XR8                     NO, CHECK NEXT                           
*                                                                               
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         GOTO1 WRITE                    RESTORE P-KEY                           
         B     XR8                      CHECK NEXT RECORD                       
         DROP  R4                                                               
*                                                                               
* FOR XRECADD AND XRECPUT                                                       
XR9      MVC   KEY(13),SAVEKEY                                                  
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
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,0(R3)                                                    
         MVC   SPVPSPV,SUPV                                                     
*                                                                               
         MVC   AIO,AIO2                 PRESERVE D/A OF INPUT REC               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
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
         L     RF,AIO                                                           
         CLI   0(RF),X'06'         AGY RECORD?                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 PUTREC                   WRITE RECORD W/ DELETED CLIENTS         
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
         CLI   ACTNUM,ACTDEL                                                    
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
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         L     RF,AIO                                                           
         OI    15(RF),X'80'                                                     
*                                                                               
         L     RF,AIO                                                           
         CLI   0(RF),X'06'         AGY RECORD?                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 PUTREC                                                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* SPECIAL CASE FOR RESTORING SUPERVISOR RECORDS                                 
*                                                                               
RCRST    NTR1                                                                   
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         NI    KEY+13,X'FF'-X'80'       TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         L     RF,AIO                                                           
         NI    15(RF),X'FF'-X'80'                                               
*                                                                               
         L     RF,AIO                                                           
         CLI   0(RF),X'06'         AGY RECORD?                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 PUTREC                                                           
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
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRACT   DS    0H                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVC   GTMTYP,GTMERR                                                    
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
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
**** ROUTINE TO VALIDATE OFFICE CODE ****                                       
*                                                                               
VALOFF   NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   VLOFX                                                            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
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
*                                                                               
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
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0B00000000030000'                                  
         LR    R5,R2                    BACKUP R2 BEFORE VALICLT                
         LA    R2,TEMPFLD                                                       
         MVC   8(3,R2),CLTCODE                                                  
         OI    TRNSTAT1,NOVALERR                                                
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALICLT                  ALSO HAS A DM CALL                      
*                                                                               
         TM    TRNSTAT1,BASERR                                                  
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
*                                                                               
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2(13),0(R6)  SAVE AWAY SUPV KEY                           
*                                                                               
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
*                                                                               
         LA    R4,KEY                   USE PAS KEY TO SUPV BY CLIENT           
         USING SPVPASS,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVPTYP,=X'0DE1'                                                 
         MVC   SPVPAM,BAGYMD                                                    
         MVC   SPVPOFC,OFFICE                                                   
         MVC   SPVPCLT,CLTCODE                                                  
         MVC   SPVPSPV,SUPV                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         L     R6,AIO                                                           
         MVC   0(13,R6),SAVEKEY2                                                
*                                                                               
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
CLTPSC10 ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
CLTPSC20 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLTPSC10            NO                                           
CTSX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR BOTTOM SCREEN - CLEARS LIST DISPLAY - NOT SELECT FIELDS          
***********************************************************************         
*                                                                               
CLBTSC   NTR1                      CLEARS SCREEN FROM ADD CLI DOWN              
         LA    R2,SPCSELH        1ST LINE                                       
         LA    R3,SPCPFKYH                                                      
*                                                                               
CLBTSC10 TM    1(R2),X'20'              IS FIELD PROTECTED?                     
         BO    CLBTSC15                 PROTECTED, CLEAR                        
         TM    6(R2),X'20'              IS FIELD MARKED FOR PROT?               
         BNO   CLBTSC20                 NO, CHECK NEXT                          
CLBTSC15 ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
CLBTSC20 OI    6(R2),X'80'         TRANSMIT                                     
****** MAKES SURE APPROPRIATE FIELDS ARE TRANSMITTED AS UNPROTECTED             
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLBTSC10            NO                                           
CBSX     B     XIT                                                              
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
         ZIC   R0,0(R2)                                                         
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
         MVI   PFAID,4                  FORCE SUPV PF KEY                       
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
         MVC   MPF04ACT,=C'       '                                             
         CLC   =C'SELECT',CONACT                                                
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
*                                                                               
         GOTO1 VINITPF,DMCB,MPFTABLE                                            
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
*                                                                               
         MVC   MPF02ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF02ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF04ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF06ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF06ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF08ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF08ACT,=CL8'DISPLAY'                                           
*                                                                               
*        CLI   CALLSP,0             ANYTHING WAITING TO RETURN TO?              
         LR    RE,RA                                                            
         AH    RE,=Y(TWAENDLQ-2)                                                
         CLI   1(RE),0                                                          
         BE    *+8                  NO                                          
         NI    SPCREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    SPCREH+6,X'80'       TRANSMIT THE RESULT                         
*                                                                               
SETUP99  GOTO1 VINITPF,DMCB,MPFTABLE                                            
SETUPX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
MPFTABLE DS    0X                                                               
* PF02 = OFFICE                                                                 
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYGRP '        RECORD: OFFICE                               
MPF02ACT DC    CL8'       '        ACTION:                                      
MPF02    DC    AL1(KEYTYTWA,L'SPCOFF-1),AL2(SPCOFF-T31CFFD)                     
MPF02X   EQU   *                                                                
*                                                                               
* PF04 = BUYER                                                                  
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYER  '        RECORD: BUYER                                
MPF04ACT DC    CL8'       '        ACTION:                                      
MPF04    DC    AL1(KEYTYTWA,L'SPCOFF-1),AL2(SPCOFF-T31CFFD)                     
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
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
CLHEAD   DC    CL79'     CODE CLIENT NAME           FLT           CODE +        
               CLIENT NAME           FLT'                                       
BUHEAD   DC    CL79'SEL  CODE BUYER NAME                     SEL  CODE +        
               BUYER NAME'                                                      
MKHEAD   DC    CL79'     MKT  MARKET NAME           BYR           MKT  +        
               MARKET NAME           BYR'                                       
*                                                                               
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'02'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'N',XL1'05'                                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA2D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA3D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENOFC                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBYR                                                       
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
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
ADDCLTS  DS    8CL3                     STORAGE FOR ADDED CLIENTS               
ADCLEND  DS    0H                       END TABLE FLAG                          
ADCLLNEQ EQU   ADCLEND-ADDCLTS          LENGTH OF TABLE                         
DELCLTS  DS    8CL3                     STORAGE FOR DELETED CLIENTS             
DECLEND  DS    0H                                                               
DECLLNEQ EQU   DECLEND-DELCLTS                                                  
DISKADD  DS    XL4                      SAVE D/A                                
SAVESEL  DS    CL1                      STORAGE FOR THISLSEL                    
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                                                              
KEY2     DS    CL50                                                             
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080NESFM4B   10/31/05'                                      
         END                                                                    
