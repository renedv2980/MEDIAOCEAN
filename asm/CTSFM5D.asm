*          DATA SET CTSFM5D    AT LEVEL 004 AS OF 04/21/16                      
*PHASE TA0A5DA                                                                  
         TITLE 'TA0A5D  STATION RATING SOURCE RECORD'                           
***********************************************************************         
*                                                                     *         
*  TITLE        TA0A5D - STATION RATING SOURCE RECORD MAINT/LIST      *         
*                                                                     *         
*  CALLED FROM  GENCON VIA TA0A00 (SFM CTFILE CONTROLLER)             *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST                   *         
*                                                                     *         
*  INPUTS       SCREEN TA0AA0 (MAINTENANCE)                           *         
*               SCREEN TA0AA1 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATE STATION RATING SOURCE RECORD                   *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'TA0A5D  STATION RATING SOURCE RECORD - INIT'                    
TA0A5D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A5D                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
         OI    GENSTAT2,DISTHSPG   STAY ON SAME PAGE OF LIST                    
*                                                                               
CKMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        PROCESS PF KEYS                              
         BNE   CKMODE1                                                          
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKMODE1                                                          
*                                                                               
         CLI   PFKEY,12            IF PFKEY 12 HIT                              
         BNE   *+16                                                             
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
         NI    GENSTAT2,X'FF'-RETEQSEL   GO BACK TO LIST SCREEN                 
         B     CKMODEX                                                          
*                                                                               
CKMODE1  DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     CKMODE90                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     CKMODE90                                                         
*                                                                               
         CLI   MODE,XRECADD        ID FILE CHANGED-RECORD ADDED                 
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD CHANGED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD DELETED                            
         BE    *+8                                                              
         CLI   MODE,XRECPUT           RECORD RESTORED                           
         BE    *+8                                                              
         B     CKMODE20                                                         
*&&DO                                                                           
         OI    GENSTAT2,USGETTXT+USMYOK USE GETTXT AND MY MSG                   
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVI   GTMTYP,GTMINF       INFORMATIONAL MESSAGE                        
*                                                                               
         LA    RF,RECYCLEQ         RECYCLE MESSAGE                              
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
         DROP  RE                                                               
*&&                                                                             
         B     CKMODEX                                                          
*                                                                               
CKMODE20 DS    0H                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   CKMODEX                                                          
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         GOTOR ERREX                                                            
*                                                                               
CKMODE90 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKMODEX                                                          
*                                                                               
         CLI   PFKEY,0             IF NO PF KEY HIT                             
         BNZ   CKMODE95                                                         
*                                                                               
         BRAS  RE,TSTNTRY             TEST IF ANY FIELD ENTERED                 
         BZ    *+12                   NO                                        
         OI    GENSTAT2,RETEQSEL      YES RETURN TO THIS SCREEN                 
         B     CKMODE95                                                         
*                                                                               
**       OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
*                                                                               
CKMODE95 DS    0H                                                               
*                                                                               
CKMODEX  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
INITERR  DS    0H                                                               
         LA    R2,CONACTH           SECURITY LOCKOUT                            
         MVI   ERROR,SECLOCK                                                    
         GOTOR ERREX                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RECYCLEQ EQU   164                 CALL COMPUTER ROOM TO RECYCLE                
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - VK'                             
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVCTSSKY,SVCTSSKY        CLEAR                                   
         LA    R4,SVCTSSKY                                                      
*                                                                               
         USING SRSKEY,R4           ESTABLISH STATION SOURCE KEY                 
*                                                                               
         MVI   SRSKSYS,SRSKSYSQ    SET RECORD MAJOR TYPE                        
         MVI   SRSKTYP,SRSKTYPQ    SET RECORD SECONDARY TYPE                    
*                                                                               
*      VALIDATE MEDIA                                                           
*                                                                               
VKMED    DS    0H                                                               
*                                                                               
         LA    R2,SCRMEDH          POINT TO MEDIA                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VKMED10             MISSING INPUT ERROR                          
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VKMEDX                                                           
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
         LA    RF,1                MAX 1 BYTE FOR CHECKING                      
VKMED10  BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         LA    R3,MEDTAB           START OF VALID MEDIAS                        
         USING MEDTABD,R3          ESTABLISH MEDIA TABLE                        
VKMED20  DS    0H                                                               
         CLI   MEDTCDE,X'FF'       INVALID IF AT END OF TABLE                   
         BE    VKINV                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),MEDTCDE     COMPARE TO ENTRY IN TABLE                    
         BE    VKMED40             MATCH                                        
*                                                                               
VKMED30  DS    0H                                                               
         LA    R3,MEDTABLQ(R3)     BUMP TO NEXT ENTRY IN TABLE                  
         B     VKMED20                                                          
*                                                                               
VKMED40  DS    0H                  MATCH TO TABLE                               
*                                                                               
         MVC   SVMED,MEDTCDE       SAVE MEDIA CODE                              
         MVC   SVMEDNAM,MEDTNAM    SAVE MEDIA NAME                              
*                                                                               
         MVC   SRSKMEDA,SVMED      ADD MEDIA TO KEY                             
VKMEDX   DS    0H                                                               
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VK99                                                             
*                                                                               
*      VALIDATE PRIORITY                                                        
*                                                                               
VKPRTY   DS    0H                                                               
         XC    SVPRTY,SVPRTY       INIT PRIORITY SAVEAREA                       
*                                                                               
         LA    R2,SCRPRIH          POINT TO PRIORITY                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VKPRTY10            MISSING INPUT ERROR                          
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VKPRTYX                                                          
         BZ    VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKPRTY10 TM    4(R2),X'08'         VALID NUMERIC?                               
         JZ    VKINV                                                            
         MVC   SVPRTY,8(R2)        SAVE PRIORITY                                
         MVC   SRSPRTY,8(R2)                                                    
*                                                                               
VKPRTYX  DS    0H                                                               
*                                                                               
*      VALIDATE STATION                                                         
*                                                                               
VKSTA    DS    0H                                                               
         XC    SVSTA,SVSTA                                                      
         LA    R2,SCRSTAH          POINT TO PRIORITY                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BNZ   VKSTA10             MISSING INPUT ERROR                          
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VKSTAX                                                           
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKSTA10  MVC   SVSTA,8(R2)                                                      
         MVC   SRSKSTTN,8(R2)                                                   
VKSTAX   DS    0H                                                               
*                                                                               
         MVC   KEY,SVCTSSKY        SET STARTING KEY                             
         GOTOR HIGH                READ FOR RECORD ON FILE                      
*                                                                               
         LA    R4,KEY              SWITCH KEY POINTER                           
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VK99                                                             
*                                                                               
VK99     DS    0H                                                               
*                                                                               
         MVC   KEY,SVCTSSKY        SET KEY                                      
*                                                                               
VKX      DS    0H                                                               
         J     EXIT                                                             
*                                                                               
VKMISS   LA    RF,MISSING          REQUIRED FIELD                               
         B     VKERR                                                            
*                                                                               
VKNOFLD  LA    RF,NOKEYFLD         NO OTHER KEY FIELDS ALLOWED                  
         B     VKERR                                                            
*                                                                               
VKNTFND  LA    RF,NOTFOUND         RECORD NOT ON FILE                           
         B     VKERR                                                            
*                                                                               
VKINV    LA    RF,INVALID          INVALID INPUT                                
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVI   GTMSYS,X'08'        REP MESSAGE SYSTEM                           
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
TOOSMALL EQU   1029                INPUT TOO SMALL                              
NOKEYFLD EQU   1030                NO OTHER KEY FIELDS ALLOWED                  
TOOBIG   EQU   1031                INPUT TOO BIG                                
PTRNOTFD EQU   1032                PARTNER NOT FOUND                            
OFCNOTOK EQU   1033                OFFICE NOT ALLOWED                           
ARSERR   EQU   1034                ONLY ONE OF PTR/STA/AGY                      
NOUSERID EQU   1035                USERID NOT ON FILE                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - DK'                             
***********************************************************************         
*                                                                     *         
*     DISPLAY  KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING SRSKEY,R4           ESTABLISH STATION SOURCE RECORD KEY          
*                                                                               
         MVC   SVCTSSKY,SRSKEY     SAVE KEY                                     
*                                                                               
         LA    RF,L'SRSKMEDA       LENGTH OF MEDIA CODE                         
         FOUT  SCRMEDH,SRSKMEDA,(RF)                                            
*                                                                               
*      INIT PTR/STA/AGY FIELDS                                                  
*                                                                               
         LA    RF,L'SRSPRTY        LENGTH OF PRIORITY CODE                      
         FOUT  SCRPRIH,SPACES,(RF)  CLEAR   PRIORITY CODE                       
*                                                                               
         LA    RF,L'SRSKSTTN       LENGTH OF STATOIN                            
         FOUT  SCRSTAH,SPACES,(RF)  CLEAR   STATION                             
*                                                                               
*      DISPLAY PRIORITY CODE                                                    
*                                                                               
         LA    RF,L'SRSPRTY        LENGTH OF PRIORITY CODE                      
         FOUT  SCRPRIH,SRSPRTY,(RF)                                             
*                                                                               
*      DISPLAY LOCAL STATION                                                    
*                                                                               
         LA    RF,L'SRSKSTTN       LENGTH OF STATOIN                            
         FOUT  SCRSTAH,SRSKSTTN,(RF)                                            
*                                                                               
DKX      DS    0H                                                               
         J     EXIT                EXIT                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - VR'                             
***********************************************************************         
*                                                                     *         
*     VALIDATE RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCTSSKY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         USING SRSRECD,R4          ESTABLISH FOUND RECORD                       
*                                                                               
*      REMOVE ALL RATING SOURCE ELEMENT                                         
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADDING RECORD                        
         BE    VRADD                                                            
*                                                                               
*      DELETE EXISTING SOURCE ELEMENTS                                          
*                                                                               
VRDEL    DS    0H                                                               
         MVI   ELCODE,RATSRCQ      SET FOR SOURCE ELEMENT                       
         GOTOR REMELEM             REMOVE ELEMENT                               
*                            NOTE R6 ==> END OF RECORD                          
VRDELX   B     VRADDX                                                           
*                                                                               
*      SET RECORD FIELDS FOR ADDING NEW RECORD                                  
*                                                                               
VRADD    DS    0H                                                               
*                                                                               
*      ADD NEW RECORD TO THE FILE                                               
*                                                                               
         MVC   KEY,SVCTSSKY        SET CURRENT KEY                              
*                                                                               
         XC    SRSRECD(100),SRSRECD  INIT RECORD BUILD AREA                     
         MVC   SRSKEY,KEY          SET KEY                                      
*                                                                               
         LA    RF,SRSELDQ          MINIMUM LENGTH OF RECORD                     
         STCM  RF,3,SRSRECLN       SET MINIMUM RECORD LENGTH                    
*                                                                               
VRADDX   DS    0H                                                               
*                                                                               
*      ADD SOURCE ELEMENT TO RECORD                                             
*                                                                               
VRSRC    DS    0H                                                               
         LA    R2,SCRSRCH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)          DID THEY ENTER ANYTHING?                     
         BZ    VRMISS                                                           
*                                                                               
         LA    R6,ELEMENT          POINT TO ELEMENT BUILD AREA                  
         XC    ELEMENT,ELEMENT                                                  
         USING RATSRCD,R6          ESTABLISH FEATURES ELEMENT                   
         MVI   RATSRC,RATSRCQ      X'10' - SET ELEM TYPE                        
         LA    R1,RATSRCOV(RE)     ADD ELEM OVERHEAD AND                        
         STC   R1,RATSRCLN         SET THE TOTAL ELEM LENGTH                    
*                                                                               
         OC    SCRSRC,SPACES                                                    
         LA    R3,SCRSRC           R3 = A(CURRENT ENTRY)                        
         LA    RE,0(R3,RE)         RE = A(END OF LIST)                          
*                                                                               
VRSRC05  DS    0H                                                               
*&&DO                                                                           
         LA    RF,SRCTAB           RF = A(LIST OF VALID SOURCES)                
VRSRC10  CLI   0(RF),X'FF'         INVALID SOURCE?                              
         BE    VRINV               YES                                          
         CLC   0(3,R3),0(RF)                                                    
         BE    VRSRC20                                                          
         AHI   RF,L'SRCTAB         BUMP TO NEXT VALID SOURCE                    
         B     VRSRC10                                                          
*&&                                                                             
*                                                                               
* CONFIRM THERE ARE NO DUPLICATES                                               
*                                                                               
VRSRC20  LA    RF,SCRSRC           POINT TO LIST ON SCREEN                      
VRSRC25  CR    RF,RE               NO MORE ENTRIES?                             
         JNL   *+2                  FORCE DEATH AS SOMETHING IS WRONG           
         CR    RF,R3               AT SAME ENTRY?                               
         JE    VRSRC30              YES, WE'RE GOOD                             
         CLC   0(3,R3),0(RF)       HAVE DUPLICATE?                              
         BE    VRDUP                YES, SEND ERROR                             
         AHI   RF,L'SRCTAB+1       BUMP TO NEXT ENTRY                           
         B     VRSRC25                                                          
*                                                                               
* CHECK IF MORE ENTRIES                                                         
*                                                                               
VRSRC30  CLI   3(R3),C','          COMMA? MORE SOURCES?                         
         BNE   VRSRC40                                                          
         AHI   R3,4                BUMP TO NEXT SOURCE                          
         B     VRSRC05             AND VALIDATE IT                              
*                                                                               
* CHECK IF END OF LIST                                                          
*                                                                               
VRSRC40  CLI   3(R3),C' '          END OF INPUT LIST?                           
         BNE   VRINV                                                            
         LA    RF,3(R3)                                                         
         CR    RF,RE               ARE WE REALLY AT END OF LIST?                
         BNE   VRINV                                                            
*                                                                               
VRSRC50  LLC   RF,5(R2)            GET LENGTH OF INPUT STRING                   
         BCT   RF,VRSRC55          SETUP FOR EX                                 
         MVC   RATSOURC(0),8(R2)                                                
VRSRC55  EX    RF,*-6                                                           
*                                                                               
         GOTOR ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
         B     VRERR                                                            
*                                                                               
VRDUP    MVI   ERROR,93            DUPLICATE ENTRY                              
         B     VRERR                                                            
*                                                                               
VRMISS   MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         GOTOR ERREX                                                            
         LTORG                                                                  
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'TA0A5D  LIST OF VALID SOURCES'                                  
***********************************************************************         
* LIST OF VALID RATING SOURCES                                                  
***********************************************************************         
SRCTAB   DC    0CL3                                                             
         DC    C'NSI'                                                           
         DC    C'WCM'              -CHG FROM TRI TO WCM - HWON 12/28/15         
         DC    C'NAM'                                                           
         DC    C'VID'                                                           
         DC    X'FF'                                                            
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - DR'                             
***********************************************************************         
*                                                                     *         
*     DISPLAY  RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*      DISPLAY SOURCE LIST                                                      
*                                                                               
         L     R4,AIO                                                           
         LA    R4,SRSELDQ(R4)                                                   
         USING RATSRCD,R4                                                       
*                                                                               
         LLC   RF,RATSRCLN         GET ELEMENT LENGTH AND                       
         SHI   RF,RATSRCOV         SUBTRACT OVERHEAD                            
         FOUT  SCRSRCH,RATSOURC,(RF)                                            
*                                                                               
*      DISPLAY ACTIVITY                                                         
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         MVI   ELCODE,X'F1'        LOOK FOR ACTIVITY ELEMENT                    
         BRAS  RE,GETEL            FIND FIRST ELEMENT                           
         BNE   DRACTVX             NONE                                         
*                                                                               
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
*                                                                               
DRACTVX  XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(14,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
DRX      DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - XR'                             
***********************************************************************         
*                                                                     *         
*     AFTER RECORD ADDED ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
XRX      DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE RECORD - LR'                             
***********************************************************************         
*                                                                     *         
*     LIST RECORDS ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         USING SRSKEY,R4           ESTABLISH RATING SOURCE KEY                  
*                                                                               
         MVI   SRSKSYS,SRSKSYSQ    SET RECORD MAJOR TYPE                        
         MVI   SRSKTYP,SRSKTYPQ    SET RECORD SECONDARY TYPE                    
*                                                                               
         MVC   SRSKMEDA,SVMED      SET MEDIA CODE                               
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 HIGH                RE-POINT FILE                                
         GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(2),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         GOTO1 GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
*                                                                               
         LA    R2,LISTAR           ESTABLISH LIST                               
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSLINE,SPACES       INIT LINE                                    
                                                                                
         MVC   LSMED,SRSKMEDA     INIT LINE                                     
                                                                                
         MVC   LSPRTY,SRSPRTY      INIT LINE                                    
                                                                                
         MVC   LSSTA,SRSKSTTN     INIT LINE                                     
                                                                                
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,RATSRCQ      X'10'                                        
         BRAS  RE,GETEL                                                         
         JNE   LR080                                                            
*                                                                               
         USING RATSRCD,R6                                                       
         LLC   R1,RATSRCLN                                                      
         SHI   R1,RATSRCOV+1                                                    
         MVC   LSSRC(0),RATSOURC     RATING SOURCE LIST                         
         EX    R1,*-6                                                           
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    DS    0H                                                               
*                                                                               
LRX      DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
*                                                                     *         
*        TEST IF ANY FIELD ENTERED THIS TIME                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                  ALIGNMENT                                    
TSTNTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,SCRSRCH          POINT TO FIRST FIELD ON SCREEN               
*                                                                               
TNTRLOOP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF SCREEN                        
         BE    TNTRDONE                                                         
*                                                                               
         TM    1(R2),X'10'         SKIP IF PROTECTED FIELD                      
         BO    TNTRCONT                                                         
*                                                                               
         TM    4(R2),X'80'         TEST IF FIELD ENTERED THIS TIME              
         BO    TNTRNTRD                                                         
*                                                                               
TNTRCONT DS    0H                                                               
*                                                                               
         LLC   RF,0(R2)            FIELD LENGTH                                 
         LA    R2,0(RF,R2)         BUMP TO NEXT FIELD                           
         B     TNTRLOOP                                                         
*                                                                               
TNTRDONE DS    0H                  NO FIELD ENTERED THIS TIME                   
         CR    RB,RB               SET EQ CC                                    
         B     TSTNTRYX                                                         
*                                                                               
TNTRNTRD DS    0H                  SOME FIELD ENTERED                           
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
TSTNTRYX J     EXIT                                                             
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE - MEDTAB'                                
***********************************************************************         
*                                                                     *         
*        TABLE FOR VALIDATING MEDIA                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
MEDTAB   DS    0D                  TABLE OF VALID USER TYPES                    
         DC    CL1'T',CL10'TELEVISION'   TELEVISION                             
         DC    CL1'R',CL10'RADIO'        RADIO                                  
         DC    CL1'X',CL10'NTWK RADIO'   NETWORK RADIO                          
         DC    X'FF'                  END OF TABLE                              
*                                                                               
         TITLE 'TA0A5D  STATION SOURCE - MEDTABD'                               
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE FOR VALIDATING MEDIAS                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
MEDTABD  DSECT                                                                  
MEDTCDE  DS    CL1                 MEDIA CODE                                   
MEDTNAM  DS    CL10                MEDIA NAME                                   
MEDTABLQ EQU  *-MEDTABD            LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA1D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVMED    DS    CL1                 MEDIA                                        
SVMEDNAM DS    CL1                 MEDIA NAME                                   
SVPRTY   DS    CL1                 PRIORITY                                     
SVSTA    DS    CL5                 STATION                                      
SVCALL   DS    0CL4                4 LETTER CALL                                
SVBAND   DS    0C                  BAND                                         
LUPDATE  DS    CL14                BUFFER FOR DATE/TIME OF LAST UPDATE          
*                                                                               
SVCTSSKY DS    XL32                KEY SAVEAREA                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LSLINE   DS    0CL70        MEDIA PRIORITY STATION SOURCES                      
LSMED    DS    CL1                                                              
         DS    CL5                                                              
LSPRTY   DS    CL1                                                              
         DS    CL8                                                              
LSSTA    DS    CL5                                                              
         DS    CL3                                                              
LSSRC    DS    CL25                                                             
         DS    CL27                SPARE                                        
         EJECT                                                                  
         EJECT                                                                  
* GEGENSTSRC                                                                    
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDCOMFACS                                                                     
* DDFLDIND                                                                      
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENSTSRC                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDACTIVD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTSFM5D   04/21/16'                                      
         END                                                                    
