*          DATA SET SPSDE02    AT LEVEL 023 AS OF 10/10/11                      
*PHASE T23102B                                                                  
T23102   TITLE 'SPSDE02 - SPOT SUPERDESK - DOWNLOAD HEADERS'                    
T23102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSDE02*,R7,RR=R4                                              
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         ST    R4,OVRELO                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         BRAS  RE,INIT             INITIALIZE                                   
*                                                                               
         CLI   SVRCVEL+1,X'02'           AUTHORIZATION EDIT/ADD                 
         BE    RCV02H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'04'           UPDATE MARKET STATUS                   
         BE    RCV04H                    (COMPLETE, APPROVE, REJECT)            
*                                                                               
         CLI   SVRCVEL+1,X'05'           DOWNLOAD STATION DETAILS               
         BE    RCV05H                    AND UPDATE STATION DETAILS             
*                                                                               
         CLI   SVRCVEL+1,X'06'           DOWNLOAD AUTHORIZATIONS                
         BE    RCV06H                    FOR TO DO/DONE LAYOUTS                 
*                                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOAD REVISION HISTORY TAB          
         BE    RCV08H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'0B'           DOWNLOAD COMMENTS AND ATTACHM          
         BE    RCV0BH                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 02 HEADER - EDIT/ADD AN AUTHORIZATION                                 
* SVTRANT CONTAINS TYPE OF TRANSACTION  (ADD, EDIT, OR NEW VERSION)             
* AIO4 CONTAINS LIST OF MARKETS EDITTING/ADDING TO AUTHORIZATION                
* AIO4 WILL CONTAIN BMKT FOR 2 AND YMD DUE DATE FOR 3 IF MKTDUES ON             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV02H   DS    0H                                                               
         CLI   SVTRANT,C'D'              EDITING AN AUTHORIZATION?              
         BNE   R02H05                                                           
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         GOTO1 HIGH                                                             
         CLC   AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BAS   RE,SND02H                                                        
         B     EXIT                                                             
*                                                                               
R02H05   TM    FLAG2,MKTSNT+MKTDUES      WAS A SPECIFIC MARKET SENT?            
         BNZ   *+8                                                              
         OI    FLAG2,ALLMKTS             NO, THEN DEFAULT TO ALL MKTS           
*                                                                               
         OC    SVSTADT(6),SVSTADT        START/END DATES SENT?                  
         BNZ   *+10                                                             
         MVC   SVSTADT(6),SVESSTR        NO, USE ESTIMATE DATES                 
*                                                                               
* FINISH FILLING IN AUTHORIZATION KEY                                           
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         CLI   SVTRANT,NEWVERS           IF ADDING A NEW VERSION                
         BNE   *+8                                                              
         MVI   AUTKAUN,0                 THEN GET CURRENT VERSION #             
         GOTO1 HIGH                                                             
         MVC   SVAUKEY(L'AUTKEY),KEYSAVE                                        
         MVC   SVVERNM,AUTKAUN           SAVE CURRENT VERSION #                 
*                                                                               
         CLI   SVTRANT,EDTAUTH           EDITING AN AUTHORIZATION?              
         BE    R02H30                                                           
*                                                                               
         CLI   SVTRANT,ADAUTH            ADDING A NEW AUTHORIZATION?            
         BNE   R02H10                                                           
         MVI   SVVERNM,X'99'             FOR FIRST ADD, VER=99                  
         CLC   AUTKEY(AUTKAUN-AUTKEY),KEYSAVE                                   
         BNE   R02H20                                                           
         MVI   ERRORCD,2                 DUPLICATE RECORD                       
         B     SND0EH                                                           
*                                                                               
R02H10   CLI   SVTRANT,NEWVERS           ADDING A NEW VERSION?                  
         BE    *+6                                                              
         DC    H'0'                      INVALID TRANSACTION CODE               
         CLC   KEY(AUTKAUN-AUTKEY),KEYSAVE   SAME A/M,CLT,PRD,EST?              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,VALFLT                 VALIDATE FLIGHT DATES                  
         JNE   SNDERMSG                  BUY FLIGHT DATES OVERLAP               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   ERROR,=Y(RANOUTVR)                                               
         CLI   SVVERNM,0                 CHECK IF RAN OUT OF VRSN #'S           
         JE    SNDERMSG                                                         
*                                                                               
         ZAP   WORK(2),=P'0'             FINISH BUILDING KEY                    
         MVO   WORK(2),SVVERNM           CONVERT CURR VER # TO PACKED           
         SP    WORK(2),=P'1'             SUBTRACT 1 FOR NEXT VER #              
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVVERNM,WORK                                                     
R02H20   MVC   AUTKAUN,SVVERNM           NEW VERSION NUMBER                     
         MVC   SVAUKEY+AUTKAUN-AUTKEY,SVVERNM                                   
         MVI   AUTKREV,X'99'             START AT ZERO REVISIONS                
         MVI   SVAUKEY+AUTKREV-AUTKEY,X'99' RESET REV# BACK TO 99               
         MVC   KEY,SVAUKEY               RESTORE KEY                            
*                                                                               
         L     R6,AIO1                   CLEAR IO AREA                          
         ST    R6,AIO                                                           
         LR    R0,R6                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   AUTKEY,SVAUKEY                                                   
         MVC   AUTRAGYA,TWAAGY           ALPHA AGY                              
         B     R02H70                    FILL IN RECORD                         
*                                                                               
R02H30   DS    0H                                                               
         MVC   ERROR,=Y(RECNTFND)                                               
         CLC   AUTKEY(AUTKREV-AUTKEY),KEYSAVE                                   
         JNE   SNDERMSG                                                         
*                                                                               
         MVC   ERROR,=Y(EDOLDREV)        EDITTING OLD REVISION?                 
         CLC   AUTKREV,SVREVNM                                                  
         JNE   SNDERMSG                                                         
*                                                                               
         MVC   SVAUKEY(L'AUTKEY),KEY                                            
         BRAS  RE,VALFLT                 VALIDATE FLIGHT DATES                  
         JNE   SNDERMSG                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY    RE-READ CURRENT REVISION               
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         OC    SVDUEDT,SVDUEDT           SENT DUE DATE?                         
         BNZ   R02H40                    NO, THEN DIDN'T CHANGE                 
         MVC   SVDUEDT,AUDDUEDT          USE OLD ONE                            
         B     R02H50                                                           
R02H40   CLC   SVDUEDT,AUDDUEDT          DUE DATE CHANGED?                      
         BE    R02H50                    NO                                     
         MVC   OLDDUEDT,AUDDUEDT         SAVE OLD BUY DUE DATE                  
         OI    OVFLAG1,DUEDTCH                                                  
*                                                                               
R02H50   CLC   SVSTADT,AUDFLST           CHECK IF START DATE CHANGED            
         BE    *+14                                                             
         OI    OVFLAG1,STADTCH                                                  
         MVC   OLDSTADT,AUDFLST          SAVE OLD START DATE                    
*                                                                               
         TM    SVFLAG1,F1APREV           APPLY REVISION CHECKED?                
         BNO   R02H70                    NO, JUST AN EDIT                       
         CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BNE   R02H70                    NO, JUST A BUYER EDIT                  
*                                                                               
R02H60   L     R6,AIO1                                                          
         MVC   ERROR,=Y(RANOUTRV)                                               
         CLI   SVREVNM,0                 RAN OUT OF REVISION NUMBERS?           
         JE    SNDERMSG                                                         
         ZAP   WORK(2),=P'0'             CALCULATE NEXT REVISION #              
         MVO   WORK(2),AUTKREV           CONVERT TO PACKED                      
         SP    WORK(2),=P'1'             SUBTRACT 1 FOR NEXT REV #              
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVREVNM,WORK                                                     
         MVC   AUTKREV,SVREVNM                                                  
         MVC   KEY+AUTKREV-AUTKEY,SVREVNM                                       
         OI    OVFLAG1,NEWREV            SET FLAG TO ADD NEW REVISION           
*                                                                               
R02H70   BAS   RE,BLDAUTH                BUILD AUTHORIZATION RECORD             
*                                                                               
* SEND AUTH (02) AND MKT DETAILS (07) SO THAT PC CAN UPDATE DIALOG              
         TM    OVFLAG1,NOUPDATE          IF U=N OPTION, DON'T TRY READ          
         BO    EXIT                                                             
*                                                                               
         USING AUTRECD,R6                GET AUTH MARKET RECORD                 
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         XC    AUTKSTA,AUTKSTA                                                  
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         BAS   RE,DWNAUT                DOWNLOAD AUTHORIZATION                  
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    BUILD AUTHORIZATION RECORD                                 
* ON ENTRY: IF ALREADY EXISTS AUTHORIZATION REC IN AIO1, ADDRESS IN R6          
* ADD/CHANGE AUTHORIZATION RECORD                                               
* ADD MARKET LEVEL RECORDS IF THERE ARE GOALS                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BLDAUTH  NTR1                                                                   
*                                                                               
         USING AUTRECD,R6                                                       
         MVC   WORK(L'AUDRVDT),AUDRVDT   SAVE ORIGINAL REVISION DATE            
         DROP  R6                                                               
*                                                                               
         CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BNE   BAUTH70             BUYER CAN ONLY ADD ATTACHMENTS               
         TM    OVFLAG1,NEWREV            IS IT A NEW REVISION                   
         BO    BAUTH02             YES - CONTINUE                               
         CLI   SVREVNM,X'99'             IS IT AN EDIT TO ORIG REV              
         BNE   BAUTH70             NO - THEN ONLY ADDING ATTACHMENTS            
*                                                                               
* PRIMARY DATE ELEMENT                                                          
BAUTH02  MVI   ELCDLO,AUDELQ                                                    
         MVI   ELCDHI,AUDELQ                                                    
         BRAS  RE,DELEL                  DELETE AUDEL                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING AUDEL,R3                                                         
         MVI   AUDEL,AUDELQ              ELEMENT CODE                           
         MVI   AUDLEN,AUDLENQ            LENGTH                                 
         MVC   AUDRVDT,WORK              ORIGINAL REVISION DATE                 
         CLI   SVTRANT,EDTAUTH           IF EDITING, DON'T CHANGE REV           
         BNE   BAUTH10                    DATE UNLESS IT'S A NEW REV            
         TM    OVFLAG1,NEWREV                                                   
         BNO   *+10                                                             
BAUTH10  MVC   AUDRVDT,TODAY                                                    
         MVC   AUDISDT,SVISSDT                                                  
         MVC   AUDFLST,SVSTADT                                                  
         MVC   AUDFLEN,SVENDDT                                                  
         MVC   AUDDUEDT,SVDUEDT                                                 
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
* SECONDARY DATE ELEMENT                                                        
         MVI   ELCDLO,A2DELQ                                                    
         MVI   ELCDHI,A2DELQ                                                    
         BRAS  RE,DELEL                  DELETE A2DEL                           
*                                                                               
         OC    SVPPDDT(9),SVPPDDT        IF DATES ARE NULLS, DON'T ADD          
         BZ    BAUTH15                   ELEMENT                                
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING A2DELD,R3                                                        
         MVI   A2DEL,A2DELQ              ELEMENT CODE                           
         MVI   A2DLEN,A2DLENQ            LENGTH                                 
*                                                                               
         MVC   A2DPRPOS,SVPPDDT                                                 
         MVC   A2DTIME,SVTPPDT                                                  
         MVC   A2DAFFID,SVAPDDT                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
* DUE DATE SET INFO                                                             
*                                                                               
BAUTH15  TM    OVFLAG1,NEWREV            IF NOT A REVISION                      
         BO    BAUTH20                                                          
         TM    OVFLAG1,DUEDTCH           AND THE DUE DATE CHANGED?              
         BNO   BAUTH20                                                          
         CLC   OLDDUEDT,=X'500404'       IF CHANGED FROM TBD                    
         BNE   BAUTH20                                                          
*                                                                               
         MVI   ELCDLO,A3DELQ                                                    
         MVI   ELCDHI,A3DELQ                                                    
         BRAS  RE,DELEL                  DELETE A3DEL                           
*                                                                               
         XC    BLOCK,BLOCK               SAVE ORIG DUE DATE INFO                
         LA    R3,BLOCK                                                         
         USING A3DELD,R3                                                        
         MVI   A3DEL,A3DELQ              ELEMENT CODE                           
         MVI   A3DLEN,A3DLENQ            LENGTH                                 
*                                                                               
         MVC   A3DDATE,TODAY                                                    
         MVC   A3DSGRP,SVBYGRP           BUYING GRP OF PERSON EDITING           
         MVC   A3DSCODE,SVUSRCD          USER CODE OF PERSON EDITING            
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
* INFO ELEMENT                                                                  
         USING AINFELD,R6                                                       
BAUTH20  MVI   BYTE,0                                                           
         MVI   ELCODE,AINFELQ                                                   
         BAS   RE,GETEL2                                                        
         BNE   *+10                                                             
         MVC   BYTE,AINFFLAG                                                    
         MVC   WORK(6),AINFBYGR          SAVE ORIG CREATOR                      
         L     R6,AIO                                                           
         DROP  R6                                                               
*                                                                               
         USING AINFELD,R3                                                       
         MVI   ELCDLO,AINFELQ                                                   
         MVI   ELCDHI,AINFELQ                                                   
         BRAS  RE,DELEL                  DELETE AINFELQ                         
*                                                                               
         LA    R3,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
         MVI   AINFEL,AINFELQ            ELEMENT CODE                           
         MVI   AINFLEN,AINFLENQ          ELEMENT LENGTH                         
         MVI   AINFSPRQ,C'N'                                                    
         TM    SVFLAG1,F1APPRQ           APPROVAL REQUIRED?                     
         BNO   *+8                       NO                                     
         MVI   AINFSPRQ,C'Y'             YES                                    
         MVC   AINFBYBS,SVBYBAS                                                 
*                                                                               
         MVC   AINFMGWK,SVMGWKS                                                 
         MVC   AINFMG,SVMGPOL                                                   
*                                                                               
         TM    BYTE,AINFRVNA             DUE DATE REV FOR A SPEC MKT            
         BNO   *+8                                                              
         OI    AINFFLAG,AINFRVNA                                                
         TM    OVFLAG1,FLTDTCH           DID BUY FLIGHT DATES CHANGE?           
         BNO   *+8                                                              
         OI    AINFFLAG,AINFDTEX         YES, DATES EXPANDED                    
*                                                                               
         TM    SVFLAG1,F1CANMK           DID THEY CANCEL MARKETS?               
         BNO   *+8                       NO                                     
         OI    AINFFLAG,AINFRVCN         YES, TURN ON BIT                       
         TM    SVFLAG1,F1DELMK           DID THEY DELETE MARKETS?               
         BNO   *+8                       NO                                     
         OI    AINFFLAG,AINFRVDL         YES, TURN ON BIT                       
*                                                                               
         NI    AINFFLAG,X'FF'-AINFMDD                                           
         TM    FLAG2,DIFFDUES            CHANGED TO MULT DIFF DUE DATES         
         BNO   *+8                                                              
         OI    AINFFLAG,AINFMDD                                                 
*                                                                               
         OI    AINFFLAG,AINFRVAL         THIS REV APPLIES TO ALL MKTS           
         CLI   SVTRANT,ADAUTH            IF ADDING, REV APPLIES TO ALL          
         BE    BAUTH40                   MARKETS                                
         TM    FLAG2,ALLMKTS             OTHERWISE, CHECK FLAG                  
         BNO   BAUTH30                   NO                                     
         TM    OVFLAG1,DUEDTCH           YES,WAS THE DUE DATE CHANGED?          
         BNO   BAUTH40                   NO                                     
         NI    AINFFLAG,X'FF'-AINFRVNA   DUE DATE CHANGED FOR ALL MKTS          
         B     BAUTH40                    SO TURN OF THIS BIT                   
*                                                                               
BAUTH30  NI    AINFFLAG,X'FF'-AINFRVAL   REV DOESN'T APPLY TO ALL MKTS          
         TM    OVFLAG1,DUEDTCH           WAS THE DUE DATE CHANGED?              
         BNO   BAUTH40                   NO                                     
         OI    AINFFLAG,AINFRVNA         REV TO DUE DATE FOR A SPEC MKT         
*                                                                               
BAUTH40  DS    0H                                                               
         CLI   SVTRANT,EDTAUTH           IF EDITING,                            
         BNE   BAUTH42                                                          
         TM    OVFLAG1,NEWREV            NEW REV GETS NEW PERSON                
         BO    BAUTH42                                                          
         MVC   AINFBYGR(6),WORK          SAME REV KEEP ORIGINAL CREATOR         
         B     BAUTH44                                                          
*                                                                               
BAUTH42  MVC   AINFBYGR,SVBYGRP          BUYING GRP OF PERSON EDITING           
         MVC   AINFUSCD,SVUSRCD          USER CODE OF PERSON EDITING            
*                                                                               
BAUTH44  GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
* COMMENT ELEMENT                                                               
         USING ACOMELD,R3                                                       
         MVI   ELCDLO,ACOMELQ                                                   
         MVI   ELCDHI,ACOMELQ                                                   
         BRAS  RE,DELEL                  DELETE ACOMELQ                         
         LA    R3,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
         MVI   ACOMEL,ACOMELQ            ELEMENT CODE                           
*                                                                               
         LA    R2,SVATHCM                R2=A(COMMENT)                          
         LHI   R0,1                      R0 = COUNTER                           
         ZICM  R4,SVCOMLEN,2             TOTAL LEN OF APPROVAL COMMENT          
BAUTH50  CHI   R4,0                                                             
         BNH   BAUTH62                   NO COMMENT SENT                        
*                                                                               
         LR    R5,R4                     SUBTRACT 250 FROM TOTAL LENGTH         
         AHI   R4,-250                   IF LEN > 250, MOVE 250 BYTES           
         CHI   R5,250                    AT A TIME                              
         BL    BAUTH60                                                          
         LHI   R5,250                                                           
*                                                                               
BAUTH60  STC   R0,ACOMSEQ                SEQUENCE NUMBER                        
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   ACOMTXT(0),0(R2)                                                 
         AHI   R5,4                      1 FOR EX + 3 FOR ELCODE, LEN,          
         STC   R5,ACOMLEN                AND SEQUENCE NUMBER                    
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
*                                                                               
         LA    R2,250(R2)                ADVANCE TO NEXT COMMENT                
         AHI   R0,1                      INCREMENT COUNTER                      
         CHI   R0,8                                                             
         BNH   BAUTH50                                                          
         DROP  R3                                                               
*                                                                               
* SD UDEF VALUE ELEMENTS                                                        
BAUTH62  MVI   ELCDLO,UDEFELQ            X'40'                                  
         MVI   ELCDHI,UDEFELQ                                                   
         BRAS  RE,DELEL                  DELETE ALL UDEF ELEMENTS               
*                                                                               
         USING UDEFELD,R3                                                       
         LA    R3,BLOCK                                                         
*                                                                               
         LA    R2,SVUDEFS                                                       
BAUTH64  CLI   0(R2),0                   ANYMORE UDEFS?                         
         BE    BAUTH70                                                          
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVI   UDEFEL,UDEFELQ            ELEMENT CODE X'40'                     
         ZIC   R5,0(R2)                                                         
         LR    R4,R5                     SAVE LENGTH                            
         STC   R5,UDEFLEN                SAME LEN AS ELEM - VALUE+3             
         MVC   UDEFSEQ,1(R2)                                                    
         AHI   R5,-4                     SO -4 FOR EX                           
         BM    BAUTH66                   SKIP IF NO VALUE                       
         EX    R5,*+4                                                           
         MVC   UDEFTXT,3(R2)             UDEF VALUE                             
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
BAUTH66  AR    R2,R4                                                            
         B     BAUTH64                                                          
         DROP  R3                                                               
*                                                                               
* ATTACHMENT ELEMENT                                                            
BAUTH70  MVI   ELCDLO,ATCHELQ                                                   
         MVI   ELCDHI,ATCHELQ                                                   
         BRAS  RE,DELEL                  DELETE ALL ATTACHMENT ELEMENTS         
*                                                                               
         USING ATCHELD,R3                                                       
         LA    R3,BLOCK                                                         
*                                                                               
         LA    R2,SVATACH                                                       
BAUTH80  LA    R4,ATACHEND                                                      
         CR    R2,R4                     PAST LENGTH OF SVATACH?                
         BNL   BAUTH90                                                          
         CLI   0(R2),0                   ANYMORE ATTACHMENTS?                   
         BE    BAUTH90                                                          
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVI   ATCHEL,ATCHELQ            ELEMENT CODE                           
         ZIC   R5,0(R2)                                                         
         LR    R4,R5                     SAVE LENGTH                            
         AHI   R5,1                      ADD 1 TO LEN FOR ELCODE                
         STC   R5,ATCHLEN                                                       
         AHI   R5,-3                     LEN OF FILENAME - 1                    
         EX    R5,*+4                                                           
         MVC   ATCHFLNM,1(R2)            ATTACHMENT FILENAME                    
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         AR    R2,R4                                                            
         B     BAUTH80                                                          
         DROP  R3                                                               
*                                                                               
* UPDATE THE RECORD                                                             
BAUTH90  CLI   SVTRANT,EDTAUTH                                                  
         BNE   BAUTH100                                                         
         CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BNE   BAUTH110            BUYERS ADD ATTACH BUT NOT A REVISION         
         TM    OVFLAG1,NEWREV                                                   
         BNO   BAUTH110                                                         
         MVI   ELCDLO,AMKTAELQ                                                  
         MVI   ELCDHI,AMKTRELQ                                                  
         BRAS  RE,DELEL                  DELETE AMKTAELQ & AMKTRELQ             
         MVI   BYTE,AMKTRELQ                                                    
BAUTH100 BAS   RE,ADDAUTH                                                       
         B     BAUTH120                                                         
*                                                                               
BAUTH110 GOTO1 PUTREC                                                           
         CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BNE   BAUTHX                    BUYER CAN ONLY ADD ATTACHMENTS         
*                                                                               
         CLI   SVREVNM,X'99'             IS IT AN EDIT TO ORIG REV              
         BNE   BAUTHX              NO - THEN ONLY ADDING ATTACHMENTS            
*                                                                               
         OC    KEY+36(4),KEY+36          TEST RUNNING W/O FILE UPDATE           
         BNZ   *+8                        (NO DISK ADDRESS)                     
         OI    OVFLAG1,NOUPDATE                                                 
         BRAS  RE,RDAMKT                 REVISE ALL ACTIVE MARKETS              
*                                                                               
         TM    OVFLAG1,DUEDTCH           CHECK DUE DATE CHANGED                 
         BNO   BAUTH115                                                         
         GOTO1 =A(ADDPASSD),RR=OVRELO    YES, CHANGE PASSIVE KEY                
*                                                                               
BAUTH115 TM    OVFLAG1,STADTCH           CHECK START DATE CHANGED               
         BNO   BAUTH120                                                         
         GOTO1 =A(ADDPASS),RR=OVRELO     YES, CHANGE PASSIVE KEY                
BAUTH120 GOTO1 =A(CHKGOA),RR=OVRELO      CHECK GOALS FOR MARKETS                
BAUTHX   B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    DOWNLOAD AUTHORIZATION                                     
* DOWNLOAD AUTHORIZATIONS THAT THE USER IS RESPONSIBLE FOR.  HOWEVER,           
* IF USER IS EDITING OR ADDING AUTHORIZATION (02) AND THEY ARE NOT              
* RESPONSIBLE, SEND A FLAG INSTEAD TO NOTIFY PC THAT ADD/EDIT IS DONE.          
*                                                                               
* AUTHORIZATION RECORD IN AIO1                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DWNAUT   NTR1                                                                   
*                                                                               
         TM    BRKFLAG,BRKRESM           TEST IF RESUMING                       
         BNO   DWNA02                                                           
         CLI   LASTHDR,7                 THEN FIGURE OUT WHERE IT LEFT          
         BNE   DWNA01                      OFF                                  
         CLI   LASTMC,0                                                         
         BE    DWNA30                                                           
         B     DWNA40                                                           
*                                                                               
DWNA01   DS    0H                                                               
         CLI   LASTHDR,2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DWNFLAG,SENT02            ALREADY SENT AUTHORIZATION             
         B     DWNA10                                                           
*                                                                               
****     TM    DWNFLAG,MYCLIENT          IS CLIENT "MY CLIENT"?                 
****     BO    DWNA10                                                           
****     BNO   DWNA10                                                           
*                                                                               
DWNA02   DS    0H                                                               
         USING AUTRECD,R6                                                       
***      CLI   SVRCVEL+1,X'02'           AUTHORIZATION EDIT/ADD?                
***      BNE   DWNA05                                                           
         GOTO1 =A(CHKCLT),RR=OVRELO      YES,CHECK IF SUPV'S CLIENT             
DWNA05   TM    DWNFLAG,MYCLIENT          IS CLIENT "MY CLIENT"?                 
         BNO   DWNA10                    NO, NEED TO CHECK MKT NOW              
         BAS   RE,SND02H                 YES, SEND AUTHORIZATION                
         BNE   NEXIT                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
*                                                                               
* READ MARKET LEVEL RECORDS                                                     
DWNA10   LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         MVI   AUTKREV,X'FF'             PASS OVER OTHER REVISIONS              
         GOTO1 HIGH                      TO GET FIRST MARKET RECORD             
DWNA20   CLC   AUTKEY(AUTKMKT-AUTKEY),KEYSAVE  SAME A/M,CLT,PRD,EST,VRS         
         BNE   DWNA50                                                           
*                                                                               
         MVC   BMKT,AUTKMKT                                                     
         GOTO1 =A(CHKMKT),RR=OVRELO      CHECK IF MKT VALID                     
         BNE   DWNA40                    MKT NOT VALID - GET NEXT ONE           
*                                                                               
         TM    DWNFLAG,SENT02            ALREADY SENT AUTHORIZATION?            
         BO    DWNA30                                                           
         BAS   RE,SND02H                 NO, SEND AUTHORIZATION                 
         BNE   NEXIT                                                            
         XC    KEY,KEY                   RE-READ AUTH MARKET RECORD             
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(AUTKREV-AUTKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DWNA30   L     R6,AIO2                   GET AUTH MARKET RECORD                 
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVSDPRF+4,C'Y'            BUYERS SEE TBDS                        
         BE    DWNA38                    YES                                    
         CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BE    DWNA38                    SUPV GETS ALL                          
         CLC   MINFDUDT,=X'500404'       TBD MKT?                               
         BNE   DWNA38                    BUYERS STILL SEE RTBDS                 
         CLI   MINFRVNM,X'99'            ANY REVISION = RTBD                    
         BE    DWNA40                    NO THEN SKIP                           
*                                                                               
DWNA38   BRAS  RE,SND07H                 DOWNLOAD AUTH MARKET DETAILS           
         BNE   NEXIT                                                            
*                                                                               
DWNA40   LA    R6,KEY                    GET NEXT MARKET                        
         MVC   AUTKSTA(4),=X'FFFFFFFF'                                          
         GOTO1 HIGH                                                             
         B     DWNA20                                                           
*                                                                               
DWNA50   CLI   SVRCVEL+1,X'02'           AUTHORIZATION EDIT/ADD?                
         BNE   DWNAUTX                                                          
         TM    DWNFLAG,SENT02            ALREADY SENT AUTHORIZATION?            
         BO    DWNAUTX                                                          
*                                                                               
         LA    R1,X'0002'                NO, SEND FLAG- NOT RESPONSIBLE         
         BAS   RE,SENDH                                                         
         BNE   NEXIT                                                            
         LA    R1,MCNTRES                NOT RESPONSIBLE FLAG                   
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
         BO    NEXIT                                                            
DWNAUTX  B     EQXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ADD AUTHORIZATIONS - PASSIVE KEYS, AUTHORIZATION, AND AUTH MKT RECDS          
* CREATES LIST OF MARKETS REVISING (WHEN USER SELECTS THEM)                     
* TURNS ON SUPERDESK BIT IN ESTIMATE RECORD                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
ADDAUTH  NTR1                                                                   
*                                                                               
         GOTO1 ADDREC                                                           
         OC    KEY+36(4),KEY+36          TEST RUNNING W/O FILE UPDATE           
         BNZ   *+8                                                              
         OI    OVFLAG1,NOUPDATE                                                 
         MVC   SVAUKEY,0(R6)                                                    
*                                                                               
         XC    HALF,HALF                 COUNTER FOR RNMMKT LIST                
         XC    HALF2,HALF2               COUNTER FOR REVMKT LIST                
*                                                                               
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                   ADD '0DB9' PASSIVE KEY                 
         MVC   AUPKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD             AGENCY/MEDIA                           
         MVC   AUPKDUE,SVDUEDT           BUY DUE DATES                          
         MVC   AUPKCLT,BCLT                                                     
         MVC   AUPKPRD,BPRD                                                     
         MVC   AUPKPRD2,BPRD2                                                   
         MVC   AUPKEST,BEST                                                     
         MVC   AUPKAUN,SVVERNM                                                  
         MVC   AUPKREV,SVREVNM                                                  
         MVC   AUTKDA,DMDSKADD           PUT DISK ADDRESS IN KEY                
         MVC   WORK(L'KEY),KEY           SAVE FOR '0DC9' PASSIVE KEY            
         GOTO1 ADD                       ADD PASSIVE KEY                        
*                                                                               
         MVC   KEY,WORK                  ADD '0DC9' PASSIVE KEY                 
         MVI   AUSKSUB,AUSKSUBQ                                                 
         MVC   AUSKSTDT,SVSTADT          WITH START DATE                        
         GOTO1 ADD                       ADD PASSIVE KEY                        
         DROP  R6                                                               
*                                                                               
* REVISE ALL ACTIVE MARKETS                                                     
         CLI   SVTRANT,EDTAUTH           IF NOT AN EDIT,                        
         BNE   AAUTH40                   SKIP THIS                              
         TM    FLAG2,ALLMKTS             CHANGE ALL ACTIVE MARKETS?             
         BNO   AAUTH10                   NO, CHANGE SPECFIC MARKETS             
         BRAS  RE,RDAMKT                 READ/REVISE ALL ACTIVE MARKETS         
         B     AAUTH40                                                          
*                                                                               
* REVISE NON-ACTIVE/SPECIFIC MARKETS SENT                                       
AAUTH10  NI    OVFLAG1,X'FF'-REVACMKT    NOW REVISE NON-ACTIVE MKTS             
         L     R1,AIO1                   REVMKT LIST                            
         L     R3,AIO4                   SELECTED MKTS                          
         L     R4,AIO3                   RNMMKT LIST                            
*                                                                               
AAUTH20  CLC   =X'FFFF',0(R3)            END OF MARKET LIST?                    
         BE    AAUTH30                   YES                                    
         ZICM  R2,0(R3),2                PUT MARKET NUMBER IN R2                
         MVC   SVMKTDD,SVDUEDT                                                  
         TM    FLAG2,MKTDUES       MKT AND DUE DATE IN TABLE                    
         BNO   *+10                                                             
         MVC   SVMKTDD,2(R3)       SPECIAL MKT DUE DATE                         
         GOTO1 =A(MKTCHK),RR=OVRELO      UPDATE TABLE OF MARKETS                
         BAS   RE,MKTADD                 ADD MARKET TO ELEMENT                  
         TM    FLAG2,MKTDUES       MKT AND DUE DATE IN TABLE                    
         BO    *+12                                                             
         LA    R3,2(R3)                                                         
         B     AAUTH20                                                          
         LA    R3,5(R3)                                                         
         B     AAUTH20                                                          
*                                                                               
* FOR REVISIONS, CREATE LIST OF MARKETS REVISING                                
AAUTH30  XC    KEY,KEY                   GET AUTHORIZATION RECORD               
         LA    R6,KEY                                                           
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         LR    R0,R6                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                  R2=A(REV MARKET LIST)                  
         MVI   BYTE,AMKTRELQ                                                    
         MVC   FULL(2),HALF2                                                    
         BAS   RE,ADMKTRV                                                       
         MVI   BYTE,AMKTAELQ                                                    
         MVC   AIO,AIO3                  R2=A(NEW MARKET LIST)                  
         MVC   FULL(2),HALF                                                     
         BAS   RE,ADMKTRV                                                       
         MVC   AIO,AIO2                  ADDRESS OF RECORD CHANGING             
         GOTO1 PUTREC                                                           
*                                                                               
AAUTH40  BRAS  RE,SDBIT                  TURN ON SDESK FLAG IN EST REC          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ADD MARKET TO NEW MARKET LIST OR REVISED MARKET LIST                          
* ON ENTRY: R2 CONTAINS MARKET NUMBER, BYTE=1 IF NEW MARKET                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
MKTADD   DS    0H                                                               
         CLI   BYTE,1                                                           
         BNE   MKTADD10                                                         
         STCM  R2,3,0(R4)          PUT NEW MARKET INTO NEW MKT LIST             
         LA    R4,2(R4)                                                         
         LH    R0,HALF                                                          
         AHI   R0,2                INCREASE LEN BY MARKET LEN                   
         STH   R0,HALF                                                          
         B     MKTADDX                                                          
*                                                                               
MKTADD10 STCM  R2,3,0(R1)          PUT MARKET INTO REV MKT LIST                 
         LA    R1,2(R1)                                                         
         LH    R0,HALF2                                                         
         AHI   R0,2                INCREASE LEN BY MARKET LEN                   
         STH   R0,HALF2                                                         
MKTADDX  BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ADD MARKET REVISION ELEMENT                                                   
* ON ENTRY: AIO CONTAINS ADDRESS OF MARKET LIST                                 
*           FULL(2) CONTAINS LENGTH OF MARKET LIST                              
*           BYTE CONATINS ELEMENT CODE OF ELEMENT ADDING                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
ADMKTRV  LR    R4,RE                                                            
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING AMKTELD,R3                                                       
         MVC   AMKTEL,BYTE                                                      
         MVI   AMKTLEN,AMKTLNQ1    LENGTH WITH NO MARKETS                       
         L     R2,AIO              R2=A(MARKET LIST)                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,FULL                                                        
ADMKT10  CHI   R0,0                                                             
         BNH   ADMKTRVX            NO MORE MARKETS IN LIST                      
         LR    R5,R0               SUBTRACT 250 FROM TOTAL LENGTH               
         CHI   R5,250              AT A TIME                                    
         BL    ADMKT20                                                          
         LHI   R5,250                                                           
*                                                                               
ADMKT20  BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   AMKTMKT(0),0(R2)                                                 
         AHI   R5,3                1 FOR EX + 2 FOR ELEM CODE, LENGTH           
         STC   R5,AMKTLEN                                                       
*                                                                               
*                                                                               
         CLI   AMKTLEN,X'FE'       TRY TO CATCH BAD MKT ELEMS!                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
*                                                                               
         AHI   R5,-2               SUBTRACT FOR ELEM CODE AND LEN               
         SR    R0,R5                                                            
         LA    R2,250(R2)          ADVANCE TO REMAINING MARKETS                 
         B     ADMKT10                                                          
         DROP  R3                                                               
ADMKTRVX LR    RE,R4                                                            
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 04 HEADER - UPDATES MARKET STATUS                                     
* (APPROVE, REJECT, MARK COMPLETE)                                              
* STATAB IS A TABLE TO DETERMINE WHAT THE VALID ACTIONS ARE FOR A               
* MARKET'S CURRENT STATUS  IF ANY NEW STATUS ELEMENTS ARE ADDED, THIS           
* TABLE SHOULD BE UPDATED.                                                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV04H   DS    0H                                                               
         MVC   ERROR,=Y(INVACTN)         INVALID ACTION                         
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AUTKMKT,BMKT                                                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    AUTKSTAT,AUTKSCAN         MARKET CANCELLED?                      
         BO    *+12                                                             
         TM    AUTKSTAT,AUTKSDL          OR DELETED?                            
         BNO   R04H10                                                           
         CLI   SVAPSTA,MSTACMPQ          YES, CAN ONLY MARK COMPLETE            
         JE    R04H10                                                           
         CLI   SVAPSTA,MSTACMSQ          YES, CAN ONLY MARK COMPLETE            
         JNE   SNDERMSG                                                         
*                                                                               
R04H10   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVMKTREV,MINFRVNO         MKT REV# FOR SAVED APP HIST            
*                                                                               
         LA    R6,AUTEL            FIRST ELEMENT                                
* FIND FIRST STATUS ELEMENT (SHOULD BE LATEST STATUS)                           
R04H20   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0             NO ELEMENTS?                                 
         BE    R04H30                                                           
         CLI   0(R6),MSTAREJQ      LOWEST STATUS ELEMENT CODE                   
         BL    R04H20                                                           
         CLI   0(R6),MSTADELQ      HIGHEST STATUS ELEMENT CODE                  
         BH    R04H20                                                           
         USING MSTAELD,R6                                                       
*                                                                               
R04H30   LA    R3,STATAB           SEE IF NEW STATUS IS VALID                   
R04H40   CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                STATUS NOT FOUND IN TABLE!                   
         CLC   SVAPSTA,0(R3)       MATCH STATUS IN TABLE                        
         BE    R04H50                                                           
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     R04H40                                                           
*                                                                               
R04H50   ZIC   RE,1(R3)                                                         
         AHI   RE,-2               NUMBER OF VALID ELCODES                      
         LA    R3,2(R3)            POINT AT ELCODES                             
R04H60   CLC   MSTAEL,0(R3)        IF NO ELCODE MATCHES, ERROR                  
         BE    R04H70                                                           
         LA    R3,1(R3)                                                         
         BCT   RE,R04H60                                                        
         J     SNDERMSG                                                         
         DROP  R6                                                               
*                                                                               
R04H70   CLI   SVAPSTA,MSTACMPQ    IF COMPLETE, ERASE APPROVE, REJECT,          
         BNE   R04H80               AND COMPLETE STATUS.(6, 7, & 8)             
         CLI   SVUSRTP,C'B'        BUYER?                                       
         BE    *+8                 YES                                          
         MVI   SVAPSTA,MSTACMSQ    SUPERVISOR COMPLETE                          
*                                                                               
         BRAS  RE,SAVEHIST         SAVE APPROVAL HISTORY                        
*                                                                               
         MVI   ELCDLO,MSTAREJQ     MSTAREJQ=6                                   
         MVI   ELCDHI,MSTACMPQ     MSTACMPQ=8                                   
         BRAS  RE,DELEL                                                         
         MVI   ELCDLO,MSTACMSQ     MSTACMSQ=A                                   
         MVI   ELCDHI,MSTACMSQ     MSTACMSQ=A                                   
         BRAS  RE,DELEL                                                         
*                                                                               
R04H80   XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING MSTAELD,R3                                                       
         MVC   MSTAEL,SVAPSTA      ELEMENT CODE/STATUS                          
         MVC   MSTADATE,TODAY                                                   
         MVC   MSTABYGR,SVBYGRP    BUYING GROUP                                 
         MVC   MSTACODE,SVUSRCD    USER CODE                                    
*                                                                               
         ZICM  R5,SVCOMLEN,2                                                    
         EX    R5,*+4                                                           
         MVC   MSTACMT(0),SVATHCM                                               
         LA    R5,MSTACMT-MSTAELD(R5)                                           
         AHI   R5,1                1 FOR EX                                     
         STC   R5,MSTALEN                                                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO,BLOCK,=C'ADD=CODE'             
         DROP  R3                                                               
*                                                                               
         L     R6,AIO                                                           
         USING AUTRECD,R6                                                       
         NI    MINFFLAG,X'FF'-MINFREV     REV NO LONGER LATEST STATUS           
*                                                                               
         GOTO1 PUTREC                                                           
         BRAS  RE,SND07H                                                        
         B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* STATAB PROVIDES A LIST OF VALID CURRENT STATUSES OF A MARKET FOR              
* AN ACTION THAT THE USER WANTS TO DO                                           
* 1ST FIELD IS CURRENT ACTION USER IS ATTEMPTING                                
* 2ND FIELD IS THE LENGTH OF THE TABLE ENTRY                                    
* 3RD - ?? FIELDS ARE A LIST OF VALID CURRENT STATUSES                          
* NOTE X'00' MEANS THAT HAVING NO STATUS ELEMENTS IS VALID                      
* EX. IF THE USER IS TRYING TO MARK A MARKET COMPLETE, THE CURRENT              
* STATUS OF THE MARKET CAN BE CANCELLED, REJECTED, OR NO STATUS.                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
STATAB   DC    AL1(MSTACMPQ),X'06',AL1(MSTACANQ),AL1(MSTAREJQ),X'00'            
         DC    AL1(MSTADELQ)                                                    
*                                                                               
         DC    AL1(MSTAREJQ),X'05',AL1(MSTAAPPQ),AL1(MSTACMPQ)                  
         DC    AL1(MSTACMSQ)                                                    
*                                                                               
         DC    AL1(MSTACMSQ),X'06',AL1(MSTACANQ),AL1(MSTAREJQ),X'00'            
         DC    AL1(MSTADELQ)                                                    
*                                                                               
         DC    AL1(MSTAAPPQ),X'04',AL1(MSTACMPQ),AL1(MSTACMSQ)                  
         DC    X'FF'                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*              AUTHORIZATION STATION DETAILS                                    
* ON ENTRY: IF BSTA = 0 THEN DOWNLOAD ALL STATION DETAILS                       
*           IF BSTA <> 0 THEN UPDATE STATION DETAILS OF STATION IN BSTA         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV05H   DS    0H                                                               
         OC    BSTA,BSTA           IF STATION IS ZERO                           
         BNZ   R05H20              READ FIRST STATION FOR THE MARKET            
         MVI   BSTA+2,1                                                         
         OI    OVMFLAG2,SNDDTLS                                                 
         B     R05H20                                                           
*                                                                               
         USING AUTRECD,R6                                                       
R05H20   LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,SVAUKEY                                                      
         MVC   AUTKMKT(5),BMKTSTA                                               
         MVC   SVAUKEY,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         TM    OVMFLAG2,SNDDTLS          SENDING ALL STATIONS' DETAILS?         
         BNO   R05H70                    NO, UPDATING A STATION'S DTLS          
         BAS   RE,SND05H                                                        
         B     RCV05HX                   YES                                    
*                                                                               
R05H70   CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R05H80   MVC   SVAUKEY,KEY               SAVE STATION KEY                       
         USING AUTRECD,R6                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                    GET STATION RECORD                     
*                                                                               
         CLI   SDTLEL,SDTLELQ            ELEMENT ALREADY EXISTS?                
         BNE   R05H100                                                          
         CLI   SDTLLEN,SDTLLENQ          LEN IS CORRECT?                        
         BE    R05H85                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         ZIC   R1,SDTLLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BLOCK(0),SDTLEL                                                  
*                                                                               
         MVI   SDTLEL,X'FF'                                                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'FF',AIO),0,0                    
         B     R05H105                                                          
*                                                                               
* MANUALLY UPDATING SENT DATE, TURN OFF FLAGS THAT OTHER PRGM UPDATED           
R05H85   OC    SVORSDT,SVORSDT                                                  
         BZ    R05H90                                                           
         MVC   SDTLORSN,TODAY            ORDER SEND DATE                        
         NI    SDTLFLG,X'FF'-SDTLDROR-SDTLBUOR-SDTLDXOR                         
*                                                                               
* MANUALLY UPDATING CNFRMD DATE, TURN OFF FLAGS THAT OTHER PRGM UPDATED         
R05H90   OC    SVORCNF,SVORCNF                                                  
         BZ    R05H95                                                           
         MVC   SDTLSCNF,TODAY            CONTRACT CONFIRMED DATE                
         NI    SDTLFLG,X'FF'-SDTLDRCN-SDTLBUCN-SDTLBYCN                         
*                                                                               
R05H95   OC    SVCNCDT,SVCNCDT                                                  
         BZ    R05H130                                                          
         MVC   SDTLCNCK,TODAY            CONTRACT CHECKED                       
         MVC   SDTLUSRC,SVUSER2          MARK CONTRACT CHECKED BY               
         B     R05H130                                                          
*                                                                               
R05H100  XC    BLOCK,BLOCK                                                      
R05H105  LA    R3,BLOCK                                                         
         USING SDTLEL,R3                                                        
         MVI   SDTLEL,SDTLELQ            ELEMENT CODE                           
         MVI   SDTLLEN,SDTLLENQ          LENGTH                                 
         OC    SVORSDT,SVORSDT                                                  
         BZ    R05H110                                                          
         MVC   SDTLORSN,TODAY            ORDER SEND DATE                        
R05H110  OC    SVORCNF,SVORCNF                                                  
         BZ    R05H115                                                          
         MVC   SDTLSCNF,TODAY            ORDER CONFIRMED                        
R05H115  OC    SVCNCDT,SVCNCDT                                                  
         BZ    R05H120                                                          
         MVC   SDTLCNCK,TODAY            CONTRACT CHECKED                       
         MVC   SDTLUSRC,SVUSER2          MARKET CONTRACT CHECKED BY             
*                                                                               
R05H120  GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
R05H130  DS    0H                                                               
* COMMENT ELEMENT                                                               
         USING SCOMELD,R3                                                       
         MVI   ELCDLO,SCOMELQ                                                   
         MVI   ELCDHI,SCOMELQ                                                   
         BRAS  RE,DELEL                  DELETE SCOMELQ                         
         LA    R3,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
         MVI   SCOMEL,SCOMELQ            ELEMENT CODE                           
*                                                                               
         LA    R2,SVATHCM                R2=A(COMMENT)                          
         LHI   R0,1                      R0 = COUNTER                           
         ZICM  R4,SVCOMLEN,2             TOTAL LEN OF APPROVAL COMMENT          
R05H140  CHI   R4,0                                                             
         BNH   R05H160                   NO COMMENT SENT                        
*                                                                               
         LR    R5,R4                     SUBTRACT 250 FROM TOTAL LENGTH         
         AHI   R4,-250                   IF LEN > 250, MOVE 250 BYTES           
         CHI   R5,250                    AT A TIME                              
         BL    R05H150                                                          
         LHI   R5,250                                                           
*                                                                               
R05H150  STC   R0,SCOMSEQ                SEQUENCE NUMBER                        
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   SCOMTXT(0),0(R2)                                                 
         AHI   R5,4                      1 FOR EX + 3 FOR ELCODE, LEN,          
         STC   R5,SCOMLEN                AND SEQUENCE NUMBER                    
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),BLOCK,=C'ADD=CODE'            
*                                                                               
         LA    R2,250(R2)                ADVANCE TO NEXT COMMENT                
         AHI   R0,1                      INCREMENT COUNTER                      
         CHI   R0,8                                                             
         BNH   R05H140                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
R05H160  GOTO1 PUTREC                                                           
*                                                                               
         XC    SVORSDT(6),SVORSDT                                               
         USING AUTRECD,R6                GET AUTH MARKET RECORD                 
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         XC    AUTKSTA,AUTKSTA                                                  
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,SND07H                 SEND MARKET DETAILS                    
RCV05HX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 05 HEADER - AUTHORIZATION STATION DETAILS                                
* ON ENTRY:                                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND05H   NTR1                                                                   
         MVI   BYTE,C'Y'                                                        
         USING AUTRECD,R6                                                       
         B     S05H20                                                           
S05H10   MVI   XSP,C'Y'                                                         
         GOTO1 SEQ                                                              
S05H20   CLC   KEY(AUTKSTA-AUTRECD),KEYSAVE                                     
         BNE   EXIT                                                             
         MVC   SVAUKEY(L'AUTKEY),KEY                                            
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   BMKTSTA,AUTKMKT     SAVE BINARY MARKET AND STATION               
         GOTO1 =A(STAUNPK),RR=OVRELO   GET STATION CALL LETTERS                 
*                                                                               
         MVC   0(8,R4),WORK        SEND STATION CALL LETTERS                    
         LA    R4,7(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
         CLI   SDTLEL,X'01'                                                     
         BE    S05H30                                                           
         BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         B     S05H80                                                           
*                                                                               
S05H30   MVC   WORK2(3),SDTLORSN   ORDER SENT DATE                              
         BRAS  RE,SETDATE                                                       
*                                                                               
         MVC   WORK2(3),SDTLSCNF   ORDER CONFIRMED DATE                         
         BRAS  RE,SETDATE                                                       
*                                                                               
* FIGURE OUT WHICH PROGRAM UPDATED ORDER SENT DATE                              
         MVI   0(R4),C'D'          DARE                                         
         TM    SDTLFLG,SDTLDROR                                                 
         BO    S05H40                                                           
         MVI   0(R4),C'U'          SPOT BUY UPLOAD                              
         TM    SDTLFLG,SDTLBUOR                                                 
         BO    S05H40                                                           
         MVI   0(R4),C'X'          DX REPORT                                    
         TM    SDTLFLG,SDTLDXOR                                                 
         BO    S05H40                                                           
         MVI   0(R4),C'M'          MANUAL (THROUGH SUPERDESK)                   
S05H40   BAS   RE,SETDLM                                                        
*                                                                               
* FIGURE OUT WHICH PROGRAM UPDATED ORDER CONFIRMED DATE                         
         MVI   0(R4),C'D'          DARE                                         
         TM    SDTLFLG,SDTLDRCN                                                 
         BO    S05H50                                                           
         MVI   0(R4),C'U'          SPOT BUY UPLOAD                              
         TM    SDTLFLG,SDTLBUCN                                                 
         BO    S05H50                                                           
         MVI   0(R4),C'B'          BUYER CONFIRMED (OM)                         
         TM    SDTLFLG,SDTLBYCN                                                 
         BO    S05H50                                                           
         MVI   0(R4),C'M'          MANUAL (THROUGH SUPERDESK)                   
S05H50   BAS   RE,SETDLM                                                        
*                                                                               
         MVC   WORK2(3),SDTLCNCK                                                
         BRAS  RE,SETDATE                                                       
*                                                                               
         CLI   SDTLUSRC,0                                                       
         BE    S05H80                                                           
*                                                                               
         CLC   SVUSER,SDTLUSRC                                                  
         BE    S05H70                                                           
         MVC   SVUSER,SDTLUSRC                                                  
         OI    FLAG1,NAMEONLY                                                   
*                                                                               
         GOTO1 VALISPV                                                          
         BE    S05H60                                                           
         GOTO1 VALIBYR                                                          
         BE    S05H60                                                           
         MVC   SVUSRNM,SDTLUSRC+2        USE CODE IF RECORD NOT THERE           
S05H60   NI    FLAG1,X'FF'-NAMEONLY                                             
*                                                                               
         MVI   XSP,C'Y'                                                         
         XC    KEY,KEY                   RE-ESTABLISH LAST RECORD               
         MVC   KEY,SVAUKEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
S05H70   MVC   0(L'SVUSRNM,R4),SVUSRNM                                          
         LA    R4,L'SVUSRNM-1(R4)                                               
S05H80   BRAS  RE,SETDLM                                                        
*                                                                               
         CLI   BYTE,C'Y'                                                        
         BNE   S05H85                                                           
         LA    R1,X'0005'                SEND 05 HEADER                         
         BAS   RE,SENDH                                                         
         MVI   BYTE,C'N'                                                        
*                                                                               
S05H85   LA    R1,MCDLSTR                DELIMITTED DOWNLOAD                    
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         MVI   ELCODE,SCOMELQ                                                   
         BRAS  RE,GETEL2                 IS THERE A COMMENT?                    
         BNE   S05H10                    NO                                     
*                                                                               
         USING SCOMELD,R6                                                       
         LA    R1,MCSTACM                                                       
         ZIC   R5,SCOMLEN                                                       
         AHI   R5,-3                                                            
         LA    R4,SCOMTXT                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         B     S05H10                                                           
         DROP  R6                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*              DOWNLOAD AUTHORIZATIONS                                          
* ON ENTRY: FOR TO DO LAYOUT, SVSTADT & SVENDDT HAVE RANGE OF DUE DATES         
*           OF AUTHORIZATIONS DOWNLOADING                                       
*           IF SVSTDUE=S GET TODO AUTHS BY START DATE - VER 3                   
*           FOR DONE LAYOUT, SVSTADT & SVENDDT HAVE RANGE OF START              
*           DATES OF AUTHORIZATIONS DOWNLOADING                                 
* READ PASSIVES (DUE DATE FOR TO DO LAYOUTS AND START DATE FOR DONE             
* LAYOUTS) AND PUT AUTHORIZATION KEY TO TSAR IF WITHIN RANGE.  THEN             
* SORT THEM BY ACTIVE AUTHORIZATION KEY ORDER.  TEST IF THE USER IS             
* RESPONSIBLE FOR THE CLIENT AND MARKET ON AUTHORIZATION, THEN DOWNLOAD         
* AUTHORIZATION AND AUTHORIZATION MARKET RECORDS.                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV06H   DS    0H                                                               
         MVI   XSP,C'Y'                                                         
         XC    BYTESNT,BYTESNT           BYTES SENT FALINK                      
         TM    BRKFLAG,BRKRESM           TEST IF RESUMING FROM BREAK            
         BNO   R06H05                                                           
         XC    KEY,KEY                   YES, RE-READ KEY                       
         MVC   KEY,LASTKEY                                                      
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLI   LASTHDR,7                 THEN FIGURE OUT WHERE IT LEFT          
         BNE   *+10                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         B     R06H70                    GO BACK TO WHERE WE WERE               
*                                                                               
* BUILD TSAR BUFFER OF RELEVANT SUPERDESK PASSIVES                              
R06H05   MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,HKEYX-HKEY                                                
         MVI   TSPAGN,12                 REQUEST 12 PAGES                       
*                                 IF NEED MORE USE BOTH TSAR BUFFERS!           
         OI    TSINDS,TSINODSK           TEMPEST IS IN USE BY FALINK !!         
         OI    TSINDS,TSIXTTWA           AND IT HAS BIG PAGES !                 
         LHI   R0,HRECX-HREC                                                    
         STH   R0,TSRECL                                                        
         LA    R1,HREC                                                          
         ST    R1,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PROCTBD,C'N'              TBD AUTHS                              
         CLC   VERSION,=X'02000000'      AFTER VERSION 2.0.0.0 ONLY             
         BNH   R06H08                                                           
         CLI   SVSDPRF+4,C'Y'            BUYERS SEE TBDS                        
         BE    R06H07                                                           
         CLI   SVUSRTP,C'S'              ELSE, ONLY FOR SPVS                    
         BNE   R06H08                                                           
R06H07   TM    FLAG2,DNLAYOUT            NOT FOR DONE                           
         BO    R06H08                                                           
         CLI   SVSTDUE,C'S'              OR TODO BY START DATE                  
         BE    R06H08                                                           
         OC    SVSTADT,SVSTADT           NO DATE SENT, SEND ALL                 
         BZ    R06H08                                                           
         MVI   PROCTBD,C'Y'              LOOK FOR TBD AUTHS FIRST               
         MVC   TEMPDTS(6),SVSTADT        SAVE REAL DATES                        
         MVC   SVSTADT,=X'500404'        TBD=4/4/80                             
         MVC   SVENDDT,=X'500404'        TBD=4/4/80                             
*                                                                               
         USING AUTRECD,R6                                                       
R06H08   LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   AUPKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD                                                    
*                                                                               
         TM    FLAG2,DNLAYOUT                                                   
         BO    R06H09                                                           
         CLI   SVSTDUE,C'S'               TODO BY START DATE                    
         BNE   *+8                                                              
R06H09   MVI   AUPKSUB,X'C9'              DONE LAYOUT PASSIVES                  
         MVC   AUPKDUE,SVSTADT                                                  
         MVI   XSP,C'Y'                                                         
R06H10   GOTO1 HIGH                                                             
*                                                                               
         CLC   AUPKEY(AUPKDUE-AUTKEY),KEYSAVE  SAME TYPE,A/M                    
         BNE   R06H15                                                           
*                                                                               
****     CLC   AUPKCLT,=X'C874'    SDU CLIENT CORRUPTED ON TST                  
***      BE    R06H40                                                           
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         OC    SVSTADT,SVSTADT           NO DATE SENT, SEND ALL                 
         BZ    R06H30                                                           
*                                                                               
* CHECK DUE DATE/START DATE FILTERS                                             
         CLC   SVSTADT,SVENDDT           IF FILTERING ON ONE DATE,              
         BNE   R06H20                    SVSTADT = SVENDDT                      
         CLC   AUPKDUE,SVSTADT                                                  
         BE    R06H30                    WANT IT                                
*                                                                               
R06H15   CLI   PROCTBD,C'Y'              ARE WE DOING TBDS NOW                  
         BNE   R06H50                    NO THEN DONE                           
         MVI   PROCTBD,C'N'                                                     
         MVC   SVSTADT(6),TEMPDTS        RESET REAL DATES                       
         B     R06H08                    AND START OVER                         
*                                                                               
R06H20   CLC   AUPKDUE,SVSTADT           DUE DATE MUST BE BETWEEN               
         BL    R06H40                    BUY DUE DATE FILTERS                   
         CLC   AUPKDUE,SVENDDT                                                  
         BNH   R06H30                                                           
         CLI   PROCTBD,C'Y'              ARE WE DOING TBDS NOW                  
         BNE   R06H50                     NO, WE ARE PAST END, SO DONE          
*                                                                               
* BUILD TSAR RECORD                                                             
R06H30   XC    TSARREC,TSARREC                                                  
         MVC   HCLT,AUPKCLT                                                     
         MVC   HPRD,AUPKPRD                                                     
         MVC   HPRD2,AUPKPRD2                                                   
         MVC   HEST,AUPKEST                                                     
         MVC   HAUN,AUPKAUN                                                     
         MVC   HREV,AUPKREV                                                     
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    R06H40                                                           
         MVC   ERROR,=Y(BUFFULL)         TOO MUCH DATA BUFFER FULL              
         J     SNDERMSG                                                         
*                                                                               
R06H40   MVI   AUPKREV,X'FF'                                                    
         B     R06H10                                                           
*                                                                               
* NOW READ TSAR RECORDS                                                         
R06H50   MVI   XSP,C'N'                                                         
         XC    LASTFLDS,LASTFLDS                                                
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
R06H60   BRAS  RE,CALLTSAR                                                      
         BNE   EXIT                                                             
         MVI   TSACTN,TSANXT                                                    
*                                                                               
         CLC   SVTSRREC(6),HREC          SAME AUTHORIZATION                     
         BE    R06H60                    YES, SKIP                              
         MVC   SVTSRREC,HREC                                                    
*                                                                               
         MVC   BCLT,HCLT                 CLIENT CODE                            
         OI    DWNFLAG,CHKLOCK           CHECK SECURITY                         
         GOTO1 =A(CHKCLT),RR=OVRELO      CHECK IF CLIENT VALID                  
         BNE   R06H60                    GET NEXT AUTH - NOT VALID              
*                                                                               
         LA    R6,KEY                    READ LATEST REV FOR AUTH               
         XC    KEY,KEY                                                          
         MVC   AUTKTYP(2),=X'0D39'                                              
         MVC   AUTKAM,BAGYMD                                                    
         MVC   AUTKCLT,HCLT                                                     
         MVC   AUTKPRD,HPRD                                                     
         MVC   AUTKPRD2,HPRD2                                                   
         MVC   AUTKEST,HEST                                                     
         MVC   AUTKAUN,HAUN                                                     
         MVC   SVAUKEY,KEY                                                      
*                                                                               
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(AUTKREV-AUTKEY),KEYSAVE                                      
         BNE   R06H60                    GET NEXT AUTH - NOT VALID              
**>      BE    *+6                 CLOSED OUT                                   
**>      DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
         NI    DWNFLAG,X'FF'-SENT02                                             
*                                                                               
*                                                                               
         CLC   AUTKREV,HREV        USE ONLY IF REVISION MATCHES                 
         BNE   R06H60              NEXT TSAR REC                                
*                                                                               
*                                                                               
R06H70   BAS   RE,DWNAUT                 DOWNLOAD AUTHORIZATION                 
         BNE   NEXIT                                                            
*                                                                               
         CLC   VERSION,=X'01010020'      VERSION 1.1.0.32 & LOW ONLY            
         BH    R06H60                                                           
         L     RE,BYTESNT                BYTES SENT FALINK                      
         C     RE,=F'300000'             DON'T PASS MAX                         
         BH    EXIT                                                             
*                                                                               
         B     R06H60                    GET NEXT TSAR RECORD                   
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 02 HEADER - AUTHORIZATION RECORD DETAILS                                 
* ON ENTRY: AIO1 CONTAINS AUTHORIZATION                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND02H   NTR1                                                                   
*                                                                               
         MVI   LASTHDR,2           SAVE IN CASE OF BREAK                        
         MVC   LASTKEY,KEY                                                      
*                                                                               
         OI    DWNFLAG,SENT02                                                   
         MVC   SVAUKEY,AUTKEY                                                   
***                                                                             
* SAVE FROM THE ORIGINAL AUTH SUPERVISOR,CODE, AND REVISON DATE                 
***                                                                             
*                                                                               
         CLI   SVRCVEL+1,X'08'           IF DOWNLOADING REV HISTORY             
         BE    S02H02                    THEN DON'T NEED ORIGINAL               
*                                                                               
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'AUTKEY),0(R6) SET UP KEY                                   
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         CLI   AUTKREV,X'99'       ORIGINAL AUTHORIZATION?                      
         BE    S02H01              YES                                          
*                                                                               
         XC    AUTKMKT,AUTKMKT     ORIGINAL AUTH HAS NO MKT!                    
         MVI   AUTKREV,X'99'       SAME KEY EXCEPT WANT ORIGINAL AUTH           
         DROP  R6                                                               
*                                                                               
S02H01   MVC   AIO,AIO3            USE AIO3                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE  SHOULD HAVE ORIGINAL AUTH REC!            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AINFELQ      X'04' - AUTHORIZATION INFO ELEMENT           
         BRAS  RE,GETEL2           MUST BE THERE                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AINFELD,R6                                                       
         MVC   SVSPRVSR,AINFBYGR   SAVE THE SUPERVISOR                          
         MVC   SVUSERCD,AINFUSCD   SAVE USER CODE                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AUDELQ       X'01' - PRIMARY DATE ELEMENT                 
         BRAS  RE,GETEL2           MUST BE THERE                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AUDEL,R6                                                         
         MVC   SVRVSNDT,AUDRVDT    SAVE THE REVISION DATE                       
         DROP  R6                                                               
*                                                                               
S02H02   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING AUTRECD,R6                                                       
         L     R6,AIO1                                                          
*                                                                               
         CLI   SVRCVEL+1,X'08'           IF DOWNLOADING REV HISTORY             
         BE    S02H30                    THEN SKIP AUTH KEY                     
*                                                                               
         GOTO1 VALICLT                                                          
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,BCLT),QCLT                               
         MVC   BPRD,AUTKPRD              PRODUCT CODE                           
         MVC   BPRD2,AUTKPRD2            PIGGYBACK PRODUCT CODE                 
         MVI   FLAG1,PRD1                                                       
         GOTO1 =A(GETPRD),RR=OVRELO                                             
         MVI   FLAG1,PRD2                                                       
         GOTO1 =A(GETPRD),RR=OVRELO                                             
*                                                                               
         CLC   LASTEST,AUTKEST           SAME ESTIMATE?                         
         BE    S02H10                                                           
         MVC   BEST,AUTKEST                                                     
         OI    FLAG1,PRD1                USE PRODUCT 1                          
         GOTO1 VALIEST                                                          
         MVC   LASTEST,AUTKEST           SAME ESTIMATE?                         
*                                                                               
S02H10   MVI   XSP,C'Y'                                                         
         MVC   0(L'QMED,R4),QMED                                                
         AHI   R4,L'QMED                                                        
         BAS   RE,SETDLM                                                        
*                                                                               
         MVC   0(L'QCLT,R4),QCLT                                                
         AHI   R4,L'QCLT                                                        
         BAS   RE,SETDLM                                                        
*                                                                               
         MVC   0(3,R4),QPRD              R5 = LENGTH                            
         AHI   R4,2                                                             
         CLI   0(R4),X'40'                                                      
         BNH   *+8                                                              
         AHI   R4,1                                                             
         OC    QPRD2,QPRD2                                                      
         BZ    S02H20                                                           
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),QPRD2                                                    
         AHI   R4,2                                                             
         CLI   0(R4),X'40'                                                      
         BNH   *+8                                                              
         AHI   R4,1                                                             
S02H20   BAS   RE,SETDLM                                                        
*                                                                               
         ZIC   R0,AUTKEST                ESTIMATE                               
         BRAS  RE,SETNUM                                                        
*                                                                               
* FIND 9'S COMP & CONVERT TO BINARY                                             
         ZAP   WORK(2),=P'0'                                                    
         MVO   WORK(2),AUTKAUN                                                  
         SP    WORK(2),=P'99'                                                   
         ZAP   DUB,WORK(2)                                                      
         CVB   R2,DUB                                                           
*                                                                               
         EDIT  (R2),(2,(R4)),ZERO=NOBLANK,ALIGN=LEFT                            
         BCTR  R0,0                                                             
         AR    R4,R0                                                            
         BAS   RE,SETDLM                                                        
*                                                                               
* FIND 9'S COMP & CONVERT TO BINARY                                             
S02H30   ZAP   WORK(2),=P'0'                                                    
         MVO   WORK(2),AUTKREV                                                  
         SP    WORK(2),=P'99'                                                   
         ZAP   DUB,WORK(2)                                                      
         CVB   R2,DUB                                                           
*                                                                               
         EDIT  (R2),(2,(R4)),ZERO=NOBLANK,ALIGN=LEFT                            
         BCTR  R0,0                                                             
         AR    R4,R0                                                            
         BAS   RE,SETDLM                                                        
*                                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOAD REVISION HISTORY              
         BE    S02H60                                                           
*                                                                               
         MVC   0(L'SVCNAME,R4),SVCNAME                                          
         LA    R4,L'SVCNAME(R4)                                                 
         BAS   RE,SETDLM                                                        
*                                                                               
         CLI   BPRD2,0                   IS THERE A PIGGYBACK?                  
         BE    S02H40                    NO                                     
         B     S02H40  *** BRANCH AROUND BECAUSE IT ISN'T WORKING!!!            
         MVC   0(10,R4),SVPNAME          YES, SEND "PRD1 NM/PRD2 NM"            
         MVI   0(R4),C'/'                                                       
         MVC   11(10,R4),SVPNAME2                                               
         LA    R4,21(R4)                                                        
         B     S02H50                                                           
*                                                                               
S02H40   MVC   0(L'SVPNAME,R4),SVPNAME                                          
         LA    R4,L'SVPNAME-1(R4)                                               
S02H50   BAS   RE,SETDLM                                                        
*                                                                               
         MVC   0(20,R4),SVEDESC                                                 
         MVI   20(R4),C'^'                                                      
         GOTO1 VDATCON,DMCB,(3,SVESSTR),(5,WORK)   EST START DATE               
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),DMCB,(3,SVESEND),(5,WORK+9)    EST END DATE                 
         MVC   21(17,R4),WORK                                                   
         LA    R4,38(R4)                                                        
         BAS   RE,SETDLM                                                        
*                                                                               
S02H60   DS    0H                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOAD REVISION HISTORY              
         BNE   S02H62                                                           
*                                                                               
         TM    REVFLAG,REVFTBD     OVERRIDE DUE DATE TO TBD TO FAKE             
         BNO   S02H61              REVISION HISTORY                             
         MVC   WORK2(3),=X'500404' TBD                                          
         B     S02H63                                                           
*                                                                               
S02H61   TM    SVINFLAG,AINFMDD    SET TO MULTIPLE DUES DATES                   
         BNO   S02H62              AGAIN FAKE REVISION HISTORY                  
         CLC   LASTVAR,=X'500405'  NEED TO ALTERNATE SO WON'T                   
         BE    S02H61B             COLLAPSE ON PC SIDE                          
         MVC   WORK2(3),=X'500405' MULTIPLE DATES                               
         MVC   LASTVAR,WORK2                                                    
         B     S02H63                                                           
S02H61B  MVC   WORK2(3),=X'500406' MULTIPLE DATES                               
         MVC   LASTVAR,WORK2                                                    
         B     S02H63                                                           
*                                                                               
S02H62   MVC   WORK2(3),AUDDUEDT                                                
S02H63   BRAS  RE,SETDATE                SEND DUE DATE                          
*                                                                               
         MVC   WORK2(3),AUDFLST                                                 
         BRAS  RE,SETDATE                SEND FLIGHT START DATE                 
*                                                                               
         MVC   WORK2(3),AUDFLEN                                                 
         BRAS  RE,SETDATE                SEND FLIGHT END DATE                   
*                                                                               
         MVC   WORK2(3),AUDISDT                                                 
         BRAS  RE,SETDATE                SEND ISSUE DATE                        
*                                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOAD REVISION HISTORY              
         BNE   S02H64                                                           
         TM    REVFLAG,REVFSUPV    OVERRIDE REVISION DATE TO FAKE               
         BNO   S02H64              REVISION HISTORY                             
         MVC   WORK2(3),SVDDDATE   SAVED DATE TBD CHANGED TO DUE DATE           
         B     *+10                                                             
S02H64   MVC   WORK2(3),AUDRVDT                                                 
         BRAS  RE,SETDATE                SEND REVISION DATE                     
*                                                                               
         LA    R6,AUTEL                                                         
         MVI   ELCODE,A2DELQ                                                    
         BRAS  RE,NEXTEL2                                                       
         BE    S02H70                                                           
         MVC   0(2,R4),=C'||'                                                   
         LA    R4,1(R4)                                                         
         BRAS  RE,SETDLM                                                        
         B     S02H80                                                           
         USING A2DELD,R6                                                        
*                                                                               
S02H70   MVC   WORK2(3),A2DPRPOS                                                
         BRAS  RE,SETDATE                                                       
*                                                                               
         MVC   WORK2(3),A2DTIME                                                 
         BRAS  RE,SETDATE                                                       
*                                                                               
         MVC   WORK2(3),A2DAFFID                                                
         BRAS  RE,SETDATE                                                       
*                                                                               
S02H80   L     R6,AIO1                                                          
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,AINFELQ                                                   
         BRAS  RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AINFELD,R6                                                       
*                                                                               
         MVC   SVINFLAG,AINFFLAG         SAVE FLAG                              
         MVC   0(1,R4),AINFBYBS          BUY BASIS                              
         BAS   RE,SETDLM                                                        
         MVC   0(1,R4),AINFMG            MAKEGOOD POLICY                        
         BAS   RE,SETDLM                                                        
         ZICM  R0,AINFMGWK,1             MAKEGOOD WEEKS                         
         BNZ   S02H90                                                           
         BAS   RE,SETDLM                                                        
         B     S02H100                                                          
S02H90   BRAS  RE,SETNUM                                                        
*                                                                               
S02H100  MVI   BYTE,C'0'                 APPROVAL REQUIRED                      
         CLI   AINFSPRQ,C'N'             0=N                                    
         BE    *+8                                                              
         MVI   BYTE,C'1'                 1=Y                                    
         MVC   0(L'BYTE,R4),BYTE                                                
         AHI   R4,L'BYTE                                                        
         BAS   RE,SETDLM                                                        
*                                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOAD REVISION HISTORY?             
         BNE   S02H120                   NOPE                                   
*                                                                               
* FOR REVISION HISTORY, PASS DOWN NAME                                          
         TM    REVFLAG,REVFSUPV    OVERRIDE SUPV TO FAKE                        
         BNO   S02H106             REVISION HISTORY                             
         MVC   SVUSER,SVDDSPV      SAVED SUPV WHO CHANGED TO DUE DATE           
         B     S02H108                                                          
*                                                                               
S02H106  CLC   SVUSER,AINFBYGR                                                  
         BE    S02H110                                                          
         MVC   SVUSER,AINFBYGR                                                  
S02H108  OI    FLAG1,NAMEONLY                                                   
         GOTO1 VALISPV                                                          
         BE    *+10                                                             
         MVC   SVUSRNM(L'AINFUSCD),AINFUSCD       USE CODE IF NO REC            
         NI    FLAG1,X'FF'-NAMEONLY                                             
         MVC   AIO,AIO1                           VALISPV MESSES UP AIO         
         MVI   XSP,C'Y'                           AND XFILE SETTING             
S02H110  MVC   0(L'SVUSRNM,R4),SVUSRNM                                          
         LA    R4,L'SVUSRNM-1(R4)                                               
         BRAS  RE,SETDLM                                                        
         B     S02H130                                                          
***                                                                             
* NOT REVISION HISTORY...GET SUPERVISORS NAME FROM SVSPRVSR                     
***                                                                             
*S02H120  CLC   SVUSER,SVSPRVSR           SAME SUPERVISOR?                      
*         BE    S02H121                   YES                                   
S02H120  MVC   SVUSER,SVSPRVSR           NO, MOVE SAVED SUPERVISOR IN           
         OI    FLAG1,NAMEONLY                                                   
         GOTO1 VALISPV                                                          
         BE    *+10                                                             
         MVC   SVUSRNM,SVUSERCD          SAVED USER CD IF REC NOT THERE         
         MVI   XSP,C'Y'                  XSPDIR/XSPFILE                         
         NI    FLAG1,X'FF'-NAMEONLY                                             
S02H121  MVC   0(L'SVUSRNM,R4),SVUSRNM                                          
         LA    R4,L'SVUSRNM-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'AINFBYGR,R4),AINFBYGR SUPERVISOR BUYING GROUP                
         LA    R4,L'AINFBYGR-1(R4)                                              
         BAS   RE,SETDLM                                                        
*                                                                               
         MVI   WORK2+2,0                                                        
         MVC   WORK2(2),SVESODT          ESTIMATE OPEN DATE                     
         BRAS  RE,SETDATE                                                       
*                                                                               
         OC    SVEDEMOS,SVEDEMOS                                                
         BZ    S02H125                                                          
         GOTO1 =A(DEMOCONV),RR=OVRELO    GET TARGET DEMO                        
         SR    R5,R5                                                            
         ICM   R5,1,WORK                 LENGTH OF DEMO                         
         BZ    S02H125                                                          
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   0(0,R4),WORK+1                                                   
         AR    R4,R5                                                            
S02H125  BAS   RE,SETDLM                                                        
*                                                                               
         MVI   WORK2+3,0                                                        
         MVC   WORK2(3),SVRVSNDT         ORIGINAL REVISION DATE                 
         BRAS  RE,SETDATE                                                       
*                                                                               
         TM    BRKFLAG,BRKRESM           TEST IF RESUMING                       
         BNO   S02H127                                                          
         CLI   LASTMC,0                  IF LASTMC=0, THEN ALREADY              
         BE    S02H130                   SENT HEADER, BUT NO MAPCODE            
*                                                                               
S02H127  LA    R1,X'0002'                SEND 02 HEADER                         
         BAS   RE,SENDH                                                         
         BNE   NEXIT                                                            
*                                                                               
S02H130  NI    BRKFLAG,X'FF'-BRKRESM                                            
         LA    R1,MCDLSTR                DELIMITTED DOWNLOAD                    
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         BO    NEXIT                                                            
*                                                                               
         BRAS  RE,SNDUDEFS               SEND UDEF VALUES                       
*                                                                               
         BRAS  RE,SND0BH                 SEND 0B W/ COMMENTS & ATTCHMNT         
*                                                                               
         CLI   SVRCVEL+1,X'08'           DOWNLOADING REV HISTORY?               
         BNE   EQXIT                                                            
*                                                                               
         LA    R1,MCENDRV                NO, MARK END OF REV HISTORY            
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
         B     EQXIT                                                            
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                         REVISION HISTORY TAB                                  
* DOWNLOADS ALL REVISIONS OF AN AUTHORIZATION AND LETS THE PC SIDE              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV08H   DS    0H                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         GOTO1 HIGH                                                             
         CLI   AUTKREV,X'99'             ORIGINAL AUTHORIZATION                 
         BNE   RCV08H10                  NO - THEN GO SEND REVS                 
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC              GET REV 0 AUTH AND CHECK FOR                 
         MVI   ELCODE,A3DELQ       X'03' - ORIG DUE DATE SET                    
         BRAS  RE,GETEL2                                                        
         BNE   EXIT                IF NOT THERE THEN NO HISTORY                 
RCV08H10 BRAS  RE,SND08H                                                        
         B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV0BH   DS    0H                                                               
         USING AUTRECD,R6                GET THE AUTHORIZATION                  
         LA    R6,KEY                                                           
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BRAS  RE,SND0BH                 SEND 0B W/ COMMENTS & ATTCHMNT         
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
EQXIT    SR    RC,RC                                                            
NEXIT    LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
         GETEL R6,24,ELCODE                                                     
         GETEL2 R6,42,ELCODE                                                    
*                                                                               
NEXTEL3  CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL3                                                          
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL3                                                          
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTEL3X LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR                    GET HEADER ADDRESS                     
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         MVI   LASTMC,0                  CLEAR LAST MAPCODE SENT                
         LR    RE,R0                                                            
         TM    BRKFLAG,BRKFULL                                                  
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                              
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         L     RE,BYTESNT                BYTES SENT FALINK                      
         AR    RE,R5                                                            
         ST    RE,BYTESNT                                                       
         STC   R1,LASTMC                 SAVE LAST MAPCODE SENT                 
         GOTO1 GETDATA                   GET DATA ITEM                          
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5                     CLEAR OVERRIDE LENGTH                  
         LR    RE,R0                                                            
         TM    BRKFLAG,BRKFULL                                                  
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* COMMON CALL TO TSAR                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                  SET CC ON EXIT                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BACK UP TO LAST NONBLANK AND INSERT PIPE DELIMITER                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SETDLM   CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLM                                                        
         MVI   1(R4),C'|'                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND A DATE IN FORMAT MMDDYY                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETDATE  DS    0H                                                               
         LR    R5,RE                                                            
         OC    WORK2(3),WORK2                                                   
         BZ    SDATE30                                                          
         CLI   WORK2+2,0                 IS THIS A COMPRESSED DATE?             
         BNE   SDATE10                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,WORK2),(X'20',WORK)  COMPRESSED                  
         B     SDATE20                                                          
*                                                                               
SDATE10  GOTO1 VDATCON,DMCB,(3,WORK2),(X'20',WORK)  BINARY                      
*                                                                               
SDATE20  MVC   0(4,R4),WORK+2                                                   
         MVC   4(2,R4),WORK                                                     
         AHI   R4,5                                                             
SDATE30  BRAS  RE,SETDLM                                                        
         LR    RE,R5                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT PIPE DELIMITER                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SETNUM   DS    0H                                                               
         EDIT  (R0),(10,(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   0(R4),C'|'                                                       
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 0E HEADER WITH ERROR CODE                                                
* ERRORCD  2=DUPLICATE AUTHORIZATION                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND0EH   LA    R1,X'000E'          SEND 0E HEADER                               
*                                                                               
         BAS   RE,SENDH                                                         
         LA    R1,MCERRCD          MAP CODE                                     
         LA    R4,ERRORCD                                                       
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         BAS   RE,SENDD                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
SNDERMSG CLI   ERRORFLG,C'A'                                                    
         JE    *+8                                                              
         MVI   ERRORFLG,0                                                       
         GOTO1 SENDMSG                                                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SOME STUFF                                                         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTT-TWAD)       A(TV MARKET LIST)                      
         ST    R1,AMKTLSTT                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTR-TWAD)       A(R MARKET LIST)                       
         ST    R1,AMKTLSTR                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTX-TWAD)       A(X MARKET LIST)                       
         ST    R1,AMKTLSTX                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(CLTLST-TWAD)        A(CLIENT LIST)                         
         ST    R1,ACLTLST                                                       
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(CLTLSTX-TWAD)       A(END OF CLIENT LIST)                  
         ST    R1,ACLTLSTX                                                      
*                                                                               
         TM    BRKFLAG,BRKRESM           TEST IF RESUMING FROM BREAK            
         BO    INITX                                                            
         MVI   OVFLAG1,0                                                        
         MVI   OVMFLAG2,0                CLEAR MARKET FLAG                      
         MVI   DWNFLAG,0                 CLEAR DOWNLOAD FLAG                    
         MVI   ERRORCD,0                                                        
*                                                                               
INITX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                  AUTHORIZATION MARKET DETAILS                                 
* DOWNLOADS MARKET LEVEL DATA                                                   
* ON ENTRY: AIO2 CONTAINS AUTH MARKET RECORD                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND07H   NTR1  BASE=*,LABEL=*                                                   
         MVI   LASTHDR,7           SAVE IN CASE OF BREAK                        
         MVC   LASTKEY,KEY                                                      
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         MVI   OVMFLAG1,0                                                       
*                                                                               
         USING AUTRECD,R6                                                       
S07H05   L     R6,AIO2                                                          
         MVC   BMKT,AUTKMKT                                                     
         MVI   BYTE2,0                                                          
         MVC   SVAUKEY,KEY                                                      
*                                                                               
         SR    R0,R0                     MARKET                                 
         ICM   R0,3,BMKT                                                        
         BRAS  RE,SETNUM                                                        
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FIRST MKT  MARKET    STATION LEVEL     STATUS  BYR      SUPVSR                
* STATUS EL  CANCELD?  RECORD EXIST? ==> TAB     COLUMN   COLUMN  VIEW          
* ---------- --------  -------------     ------  -------  ------- ----          
* CANCEL     YES       Y                 TO DO   PENDING  CANCLD  TO DO         
* CANCEL     YES       N                 DONE    BLANK    CANCLD  DONE          
* DELETED    YES       Y                 TO DO   PENDING  CANCLD  TO DO         
* DELETED    YES       N                 DONE    BLANK    CANCLD  DONE          
* COMPLETE   YES       Y/N               DONE    <DATE>   CANCLD  DONE          
* COMPLETE   NO        Y & CONFIRMED     DONE    <DATE>   PENDING TO DO         
* COMPLETE   NO        Y & NOT CFRMD     TO DO   <DATE>   PENDING TO DO         
* APPROVE    NO        N                 TO DO   <DATE>   <DATE>  TO DO         
* APPROVE    NO        Y & CONFIRMED     DONE    <DATE>   <DATE>  DONE          
* APPROVE    NO        Y & NOT CFRMD     TO DO   <DATE>   <DATE>  TO DO         
* REJECT     NO        Y/N               TO DO   PENDING  REJECTD TO DO         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         TM    AUTRSTAT,AUTRSCAN         MARKET CANCELLED?                      
         BNO   S07H09                    NO                                     
         OI    OVMFLAG1,MKTCANLD         MARKET IS CANCELLED                    
         LA    R0,MSTACANQ               SEND SPV STATUS AS CANCELLED           
         BRAS  RE,SETNUM                                                        
         B     S07H50                                                           
*                                                                               
S07H09   TM    AUTRSTAT,AUTRSDL          MARKET "DELETED"?                      
         BNO   S07H10                    NO                                     
         OI    OVMFLAG1,MKTDELD          MARKET IS DELETED                      
         LA    R0,MSTADELQ+1             SEND SPV STATUS AS DELETED             
         BRAS  RE,SETNUM                                                        
         B     S07H50                                                           
*                                                                               
* FIND FIRST STATUS ELEMENT TO DETERMINE CURRENT STATUS                         
S07H10   LA    R6,AUTEL                  FIRST ELEMENT                          
         USING MSTAELD,R6                                                       
         MVI   ELCDLO,MSTAREJQ           LOWEST STATUS ELEMENT CODE             
         MVI   ELCDHI,MSTADELQ           HIGHEST STATUS ELEMENT CODE            
         BRAS  RE,NEXTEL3                                                       
         BE    S07H20                                                           
         MVC   0(3,R4),=C'||'            NO STATUS TO SND FOR BYR & SPV         
         LA    R4,3(R4)                  & NO CANCEL DATE                       
         BRAS  RE,SETDLM                 SEND SUPERVISOR APPROVAL DATE          
         B     S07H100                                                          
*                                                                               
* FIRST STATUS ELEMENT FOUND SHOULD BE CURRENT STATUS                           
S07H20   CLI   MSTAEL,MSTAAPPQ           APPROVED?                              
         BNE   S07H30                                                           
         OI    OVMFLAG1,SUPAPVD          SUPERVISOR APPROVED FLAG               
         MVC   WORK2(3),MSTADATE                                                
         BRAS  RE,SETDATE                SEND STATUS DATE                       
         B     S07H50                                                           
*                                                                               
S07H30   LA    R0,PENDING                                                       
         CLI   MSTAEL,MSTACMPQ           IF CMPLTD, SPV STATUS IS PNDNG         
         BE    S07H40                                                           
         CLI   MSTAEL,MSTACMSQ           IF CMPLTD, SPV STATUS IS PNDNG         
         BE    S07H40                                                           
         ZIC   R0,MSTAEL                 SEND SUPERVISOR STATUS CODE            
S07H40   BRAS  RE,SETNUM                                                        
         CLI   MSTAEL,MSTAREJQ           IF RJCTD, BYR STATUS IS PNDNG          
         BE    S07H60                                                           
*                                                                               
S07H50   L     R6,AIO2                                                          
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,MSTACMPQ           COMPLETED BUYER ELEMENT?               
         BRAS  RE,NEXTEL2                                                       
         BE    S07H70                    YES                                    
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,MSTACMSQ           COMPLETED SUPERVISOR ELEMENT?          
         BRAS  RE,NEXTEL2                                                       
         BE    S07H70                    YES                                    
*                                                                               
S07H60   LA    R0,PENDING                NO, THEN BYR STATUS IS PENDING         
         LR    R3,R4                     SAVE ADRSS, MAY CHNG TO BLANK          
         OI    OVMFLAG1,BYRPNDG          BUYER STATUS IS PENDING                
         BRAS  RE,SETNUM                                                        
         B     S07H80                                                           
*                                                                               
S07H70   MVC   WORK2(3),MSTADATE                                                
         BRAS  RE,SETDATE                SEND STATUS DATE                       
*                                                                               
S07H80   L     R6,AIO2                                                          
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,MSTACANQ           IS THERE A CANCELLED ELEMENT?          
         XC    WORK2,WORK2                                                      
         BRAS  RE,NEXTEL2                                                       
         BNE   S07H82                                                           
         MVC   WORK2(3),MSTADATE                                                
         B     S07H90                                                           
*                                                                               
S07H82   L     R6,AIO2                                                          
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,MSTADELQ           IS THERE A 'DELETED' ELEMENT?          
         XC    WORK2,WORK2                                                      
         BRAS  RE,NEXTEL2                                                       
         BNE   S07H90                                                           
         MVC   WORK2(3),MSTADATE                                                
*                                                                               
S07H90   BRAS  RE,SETDATE                SEND STATUS DATE                       
*                                                                               
         USING AUTRECD,R6                                                       
S07H100  L     R6,AIO2                                                          
         MVC   WORK2(3),MINFDUDT                                                
         BRAS  RE,SETDATE                SEND MARKET DUE DATE                   
*                                                                               
         MVC   WORK2(3),MINFGIDT                                                
         BRAS  RE,SETDATE                SEND GOAL INPUT DATE                   
*                                                                               
         XC    WORK2(3),WORK2                                                   
         CLC   MINFGIDT,MINFGCDT         IF GOAL INPUT = CHANGE DATE            
         BE    *+10                      DON'T SEND A CHANGE DATE               
         MVC   WORK2(3),MINFGCDT                                                
         BRAS  RE,SETDATE                SEND GOAL CHANGE DATE                  
*                                                                               
         MVC   WORK2(3),MINFNXDT                                                
         BRAS  RE,SETDATE                BUY XFER DATE (NWS)                    
*                                                                               
         MVC   WORK2(3),MINFWKDT                                                
         BRAS  RE,SETDATE                NWS WORK ACTIVITY DATE                 
*                                                                               
         MVC   WORK2(3),MINFMLDT                                                
         BRAS  RE,SETDATE                MEDIA LOCKIN DATE                      
*                                                                               
         BRAS  RE,STNSTA                 GET STATION STATUS                     
         CLI   STAORSTA,STABLNK          NO STATIONS ORDERED?                   
         BNE   S07H110                                                          
         TM    OVMFLAG1,BYRPNDG+MKTCANLD BYR STATUS PNDG & MKT CANCLD           
         BO    *+12                      TO FORCE TO DO LAYOUT                  
         TM    OVMFLAG1,BYRPNDG+MKTDELD  BYR STATUS PNDG & MKT DELETD           
         BNO   S07H110                   TO FORCE TO DO LAYOUT                  
         MVI   STAORSTA,STABLSTA         BLANK, BUT HAS STATIONS                
****>>   MVI   0(R3),BLANK               ALWAYS STAY PENDING!                   
*                                                                               
S07H110  LA    R6,KEY                    RE-READ MARKET RECORD                  
         XC    KEY,KEY                                                          
         MVC   KEY(AUTKSTA-AUTRECD),SVAUKEY                                     
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(AUTKSTA-AUTRECD),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R6,AIO2                   POINT TO MARKET RECORD                 
*                                                                               
         ZIC   R0,STAORSTA                                                      
         CHI   R0,STACNFRM               CHECK IF STATUS IS CONFIRMED?          
         BE    S07H120                   YES, SO SEND DATE                      
         BRAS  RE,SETNUM                 NO, SO SEND STAORSTA INSTEAD           
         B     S07H130                                                          
*                                                                               
S07H120  MVC   WORK2(3),STACNFDT                                                
         BRAS  RE,SETDATE                SEND CONFIRMED DATE                    
*                                                                               
S07H130  MVC   WORK2(3),MINFBGDT                                                
         BRAS  RE,SETDATE                SEND BUYING GUIDELINES DATE            
*                                                                               
         XC    WORK2(3),WORK2                                                   
         CLI   MINFRVNM,X'99'                                                   
         BE    S07H140                                                          
         CLI   MINFRVNO,0                IF REVISION NUMBER = 0                 
         BE    S07H140                   THEN NO REVISION DATE                  
         MVC   WORK2(3),MINFRVDT                                                
S07H140  BRAS  RE,SETDATE                SEND MARKET REVISION DATE              
*                                                                               
         CLC   VERSION,=X'02000000'      AFTER VERSION 2.0.0.0                  
         BNH   S07H146                                                          
         CLC   MINFDUDT,=X'500404'       4/4/80 = TBD                           
         BNE   S07H146                                                          
         MVI   BYTE,10                   TBD STATUS                             
         CLI   MINFRVNM,X'99'            NO REVISIONS                           
         BE    S07H170                                                          
         MVI   BYTE,11                   RTBD STATUS IF REVISIONS               
         B     S07H170                                                          
*                                                                               
S07H146  MVI   BYTE,1                    EDIT/REV/ADD LATEST                    
         GOTO1 VDATCON,DMCB,(3,MINFRVDT),(0,WORK)    CONV TO CHAR               
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'6'   ADD DAYS                   
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)     CONV TO BINARY              
         CLC   TODAY,WORK                                                       
         BH    S07H150                   7 DAYS ARE UP,LOOKUP NEXT STAT         
         TM    MINFFLAG,MINFREV          IS EDIT THE LATEST STATUS?             
         BO    S07H170                   YES, DONE                              
         B     S07H160                   NO, CHECK STATUS ELEMENTS              
*                                                                               
S07H150  MVI   BYTE,0                    NO COLORS                              
S07H160  LA    R6,AUTEL                  POINT TO FIRST ELEMENT                 
         MVI   ELCDLO,MSTAREJQ           LOWEST STATUS ELEMENT CODE             
         MVI   ELCDHI,MSTADELQ           HIGHEST STATUS ELEMENT CODE            
         BRAS  RE,NEXTEL3                                                       
         BNE   S07H170                   NO STATUS ELEMENTS                     
         MVC   BYTE,0(R6)                EL CODE = LATEST STATUS                
*                                                                               
         CLI   BYTE,MSTACMSQ             SUPERVISOR COMPLETE                    
         BNE   *+8                                                              
         MVI   BYTE,MSTACMPQ             REPLACE W/ BYR COMPLETE CODE           
         CLI   BYTE,MSTADELQ             DELETED                                
         BNE   *+8                                                              
         MVI   BYTE,MSTADELQ+1           NUMBER IS ELCODE+1 =12                 
*                                                                               
         CLI   BYTE,MSTACMPQ             IF NOT COMPLETED SEND STATUS           
         BNE   S07H170                                                          
*                                        IF COMPLETE....                        
         L     R6,AIO2                   POINT TO MARKET RECORD                 
         LA    R6,AUTEL                                                         
         MVI   ELCDLO,MSTACANQ           CHECK IF CANCELLED                     
         MVI   ELCDHI,MSTACANQ                                                  
         BRAS  RE,NEXTEL3                IF NOT CANCLD,                         
         BNE   S07H168                   GIVE COMPLETED STATUS COLOR            
         MVI   BYTE,MSTACANQ             SEND CANCELLED STATUS                  
         B     S07H170                                                          
*                                                                               
S07H168  L     R6,AIO2                   POINT TO MARKET RECORD                 
         LA    R6,AUTEL                                                         
         MVI   ELCDLO,MSTADELQ           IF COMPLETED, CHECK IF DELETE          
         MVI   ELCDHI,MSTADELQ                                                  
         BRAS  RE,NEXTEL3                IF NOT DELETED                         
         BNE   S07H170                   GIVE COMPLETED STATUS COLOR            
         MVI   BYTE,MSTADELQ+1           SEND DELETED STATUS=12                 
*                                                                               
S07H170  ZIC   R0,BYTE                                                          
         BRAS  RE,SETNUM                                                        
*                                                                               
         L     R6,AIO2                                                          
         ZIC   R0,MINFRVNO               REVISION NUMBER                        
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   WORK2(3),MINFRDDT         DATE BUY RPTS DUE TO PLANNING          
         BRAS  RE,SETDATE                                                       
*                                                                               
         MVC   WORK2(3),MINFRSDT         DATE REPORTS SENT TO FIELD             
         BRAS  RE,SETDATE                                                       
*                                                                               
         CLC   VERSION,=X'02000000'      AFTER VERSION 2.0.0.0                  
         BNH   S07H179                                                          
*                                                                               
         L     R6,AIO2                   SEND STATUS COUNTS                     
         LA    R6,AUTEL-AUTKEY(R6)                                              
         MVI   ELCODE,MSCTELQ            STATUS COUNT ELEM                      
         BRAS  RE,NEXTEL2                                                       
         BNE   S07H179                                                          
*                                                                               
         USING MSCTELD,R6                                                       
         ZIC   R0,MSCTAPP                APPROVED COUNT                         
         BRAS  RE,SETNUM                                                        
         ZIC   R0,MSCTCOMP               COMPLETED COUNT                        
         BRAS  RE,SETNUM                                                        
         ZIC   R0,MSCTREJ                REJECTED COUNT                         
         BRAS  RE,SETNUM                                                        
*                                                                               
S07H179  TM    BRKFLAG,BRKRESM           TEST IF RESUMING                       
         BNO   S07H180                                                          
         CLI   LASTMC,0                  IF LASTMC=0, THEN ALREADY              
         BE    S07H190                   SENT HEADER, BUT NO MAPCODE            
*                                                                               
S07H180  LA    R1,X'0007'                SEND 07 - MARKET DETAILS               
         BAS   RE,SENDH                                                         
         BNE   NEXIT                                                            
*                                                                               
S07H190  NI    BRKFLAG,X'FF'-BRKRESM                                            
         LA    R1,MCDLSTR                DELIMITTED DOWNLOAD                    
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         BO    NEXIT                                                            
*                                                                               
SND07HX  B     EQXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE FLIGHT DATES                                       
*                                                                               
* FOR ADDING A NEW VERSION:                                                     
* READS THE CURRENT REVISION FOR EACH VERSION TO MAKE SURE THE                  
* BUY FLIGHT DATES DO NOT OVERLAP WITH OTHER VERSIONS.                          
*                                                                               
* FOR EDITING A AUTHORIZATION:                                                  
* READ THE CURRENT REVISION (OF THE SAME VERSION) TO MAKET SURE                 
* THE NEW FLIGHT DATES ARE EXPANDED, NOT SHORTENED, AND                         
* READS THE CURRENT REVISION FOR EACH VERSION TO MAKE SURE THE                  
* BUY FLIGHT DATES DO NOT OVERLAP WITH OTHER VERSIONS.                          
*                                                                               
* ON ENTRY: KEY HAS CURRENT REVISION OF CURRENT VERSION OF AUTH                 
* ON EXIT : BRANCHES TO NO IF FLIGHT DATES ARE NOT VALID                        
*           BRANCHES TO YES IF FLIGHT DATES ARE VALID                           
***********************************************************************         
VALFLT   NTR1  BASE=*,LABEL=*                                                   
         USING AUTRECD,R6                                                       
*                                                                               
VFLT10   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVTRANT,EDTAUTH           EDIT?                                  
         BNE   VFLT30                                                           
         CLC   SVVERNM,AUTKAUN           CURRENT VERSION?                       
         BNE   VFLT30                                                           
*                                                                               
         CLC   SVSTADT,AUDFLST           CHECK IF DATES CHANGED                 
         BNE   VFLT20                                                           
         CLC   SVENDDT,AUDFLEN                                                  
         BE    VFEQXIT                   DATES DIDN'T CHANGE                    
*                                                                               
VFLT20   OI    OVFLAG1,FLTDTCH           YES, DATES CHANGED.                    
         MVC   ERROR,=Y(BYFLTSHR)        CANNOT SHORTEN BUY FLT DATES           
         CLC   SVSTADT,AUDFLST           CHECK IF DATES SUBSET OF ORIG          
         BH    VFNEXIT                                                          
         CLC   SVENDDT,AUDFLEN                                                  
         BL    VFNEXIT                                                          
         MVI   KEY+AUTKAUN-AUTRECD,0                                            
         B     VFLT40                                                           
*                                                                               
VFLT30   MVC   ERROR,=Y(BYFLTOVP)        BUY FLIGHT DATES OVERLAP               
         CLC   AUDFLST,SVENDDT           MAKET SURE DATES DO NOT                
         BH    VFLT40                    OVERLAP W/ OTHER VERSIONS              
         CLC   AUDFLEN,SVSTADT                                                  
         BNL   VFNEXIT                                                          
*                                                                               
VFLT40   LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         MVC   AUTKMKT(6),=6X'FF'        GET NEXT VERSION OF AUTH REC           
         GOTO1 HIGH                                                             
         CLI   SVTRANT,EDTAUTH           EDITING AN AUTHORIZATION?              
         BNE   VFLT50                                                           
         CLC   SVAUKEY,KEY               MATCH RECORD WE ARE UPDATING?          
         BE    VFLT40                    YES, SKIP THIS ONE                     
*                                                                               
VFLT50   CLC   KEY(AUTKAUN-AUTKEY),KEYSAVE  SAME A/M,CLT,PRD,EST?               
         BE    VFLT10                                                           
         B     VFEQXIT                                                          
*                                                                               
VFEQXIT  SR    RC,RC                                                            
VFNEXIT  LTR   RC,RC                                                            
VFEXIT   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    DELETE ELEMENTS                                            
* ON ENTRY: ELCDLO IS THE LOWEST ELEMENT CODE TO DELETE                         
*           ELCDHI IS THE HIGHTEST ELEMENT CODE TO DELETE                       
*           AIO CONTAINS THE ADDRESS OF THE AUTHORIZATION RECORD                
* ON EXIT:  ALL ELEMENTS BETWEEN AND INCLUDING ELCDLO AND ELCDHI ARE            
*           DELETED                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DELEL    NTR1  BASE=*,LABEL=*                                                   
         USING AUTRECD,R6                                                       
         L     R6,AIO                                                           
         LA    R6,AUTEL                                                         
DELEL10  CLI   0(R6),0                                                          
         BE    DELEL30                                                          
         CLC   0(1,R6),ELCDLO                                                   
         BL    DELEL20                                                          
         CLC   0(1,R6),ELCDHI                                                   
         BH    DELEL20                                                          
         MVI   0(R6),X'FF'                                                      
DELEL20  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DELEL10                                                          
DELEL30  GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'FF',AIO),0,0                    
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* STATION STATUS  READS AUTH STATION RECORDS                                    
*                 AND RETURNS THE STATUS                                        
* ON ENTRY: LAST RECORD READ WAS AUTH MARKET AND FOUND IN AIO2                  
* ON EXIT:  STAORSTA CONTAINS STATION ORDER STATUS                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
STNSTA   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO2                                                          
         MVC   SVAUKEY,0(R6)             SAVE KEY OF AUTH MARKET RECORD         
         MVI   OVMFLAG2,0                                                       
         MVI   STAORSTA,STABLNK          INITIALIZE STATN ORDER STATUS          
         XC    STACNFDT,STACNFDT         LAST STATION CONFIRMED DATE            
*                                                                               
STNS10   GOTO1 SEQ                                                              
STNS20   CLC   KEY(AUTKSTA-AUTRECD),KEYSAVE                                     
         BNE   STNS60                                                           
         OI    OVMFLAG2,STAEXST                                                 
         USING AUTRECD,R6                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   AUTEL,SDTLELQ             IS THERE A STATUS ELEMENT?             
         BNE   STNS40                    NO, STATION HASN'T BEEN CNFRMD         
*                                                                               
STNS30   OC    SDTLORSN,SDTLORSN         ORDERED DATE THERE?                    
         BZ    *+12                                                             
         OI    OVMFLAG2,ORDERED          YES, STATION HAS BEEN ORDERED          
         B     *+8                                                              
         OI    OVMFLAG2,NOTORDRD         A STATION HAS NOT BEEN ORDERED         
*                                                                               
         OC    SDTLSCNF,SDTLSCNF         CONFIRMED DATE THERE?                  
         BZ    STNS50                    NO READ NEXT STATION                   
         CLC   STACNFDT,SDTLSCNF         KEEP LATEST CONFIRMED DATE             
         BNL   *+10                                                             
         MVC   STACNFDT,SDTLSCNF                                                
         OI    OVMFLAG2,CNFRMD                                                  
         B     STNS10                    YES, READ NEXT STATION                 
*                                                                               
STNS40   OI    OVMFLAG2,NOTORDRD         A STATION HAS NOT BEEN ORDERED         
STNS50   OI    OVMFLAG2,NOTCNFMD         A STATION HAS NOT BEEN CNFRMD          
         B     STNS10                    READ NEXT STATION                      
*                                                                               
* SEND STATUS FOR ALL STATIONS                                                  
* BLNK=NO STATIONS ORDERED AND NO STATIONS CONFIRMED                            
* PRTL=EITHER A STATION HAS BEEN ORDERED OR CONFIRMED - NOT ALL THOUGH          
* CNFRM=ALL STATIONS CONFIRMED, BUT NOT NECESARILY ORDERED                      
STNS60   TM    OVMFLAG2,STAEXST          DO ANY STATIONS EXIST?                 
         BNO   STNSTAX                                                          
         MVI   STAORSTA,STABLSTA         BLANK, BUT HAS STATIONS                
         TM    OVMFLAG2,ORDERED+CNFRMD   ANY STATIONS ORDERD OR CNFRMD?         
         BZ    STNSTAX                   NO, THEN BLANK W/ STATIONS             
         MVI   STAORSTA,STACNFRM         ALL CONFIRMED                          
         TM    OVMFLAG2,NOTCNFMD         WERE ANY STATIONS NOT CNFRMED?         
         BNO   STNSTAX                   NO, THEN CONFIRMED                     
         MVI   STAORSTA,STAORDRD         ALL ORDERED                            
         TM    OVMFLAG2,NOTORDRD         WERE ANY STATIONS NOT ORDERED?         
         BNO   STNSTAX                   NO, THEN ORDERED                       
         MVI   STAORSTA,STAPRTL          YES, PARTIAL                           
*                                                                               
STNSTAX  XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                         REVISION HISTORY TAB                                  
* DOWNLOADS ALL REVISIONS OF AN AUTHORIZATION AND LETS THE PC SIDE              
* DETERMINE WHAT CHANGED.  SENDS AN 08 HEADER, USING THE CODE THAT              
* SENDS AN 02 HEADER WITH AUTHORIZATION DETAILS, COMMENTS, ATTACHMENTS,         
* AND A LIST OF MARKETS EDITED, ADDED OR CANCELLED IN THE REVISION.             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND08H   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,X'0008'                SEND 08 HEADER - REV HISTORY           
         BAS   RE,SENDH                                                         
*                                                                               
S08H10   CLC   KEY(AUTKREV-AUTKEY),KEYSAVE                                      
         BNE   S08HX                                                            
         MVC   SVAUKEY,KEY                                                      
*                                                                               
         MVI   REVFLAG,0                                                        
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING AUTKEY,R6                                                        
         GOTO1 GETREC                                                           
         CLI   AUTKREV,X'99'             IF ORIG - NEED TO CHECK FOR            
         BNE   S08H18                    DUE DATE CHANGE INFO                   
         MVI   ELCODE,A3DELQ             X'03' - ORIG DUE DATE SET              
         BRAS  RE,GETEL2                                                        
         BNE   S08H18                    REGULAR HISTORY                        
         USING A3DELD,R6                                                        
         MVC   SVDDDATE,A3DDATE    SAVE DATE OF DUE DATE SETTING                
         MVC   SVDDSPV,A3DSGRP     SAVE SUPV WHO SET DUE DATE                   
         OI    REVFLAG,REVFSUPV    OVERRIDE SUPV/REV DATE                       
         L     R6,AIO                                                           
*                                                                               
         MVI   SVINFLAG,0                                                       
         MVI   ELCODE,AINFELQ            X'04' - INF ELEM                       
         BRAS  RE,GETEL2                                                        
         BNE   *+10                                                             
         USING AINFELD,R6                                                       
         MVC   SVINFLAG,AINFFLAG         SAVE FLAG                              
         L     R6,AIO                                                           
*                                                                               
         BAS   RE,SND02H           08 HEADER, MIMMICK 02 HDR                    
         NI    REVFLAG,X'FF'-REVFSUPV                                           
         OI    REVFLAG,REVFTBD     SET DUE DATE TO TBD                          
*                                  AND SEND ANOTHER REV 0                       
         LA    R1,MCMKT            APPLIED TO MKT = ALL                         
         MVI   BYTE,0                                                           
         LA    R4,BYTE                                                          
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
*                                                                               
S08H18   L     R6,AIO1                                                          
         MVI   ELCODE,AINFELQ            X'04' - INF ELEM                       
         BRAS  RE,GETEL2                                                        
         BNE   S08H18A                                                          
         USING AINFELD,R6                                                       
         MVC   SVINFLAG,AINFFLAG         SAVE FLAG                              
         L     R6,AIO                                                           
*                                                                               
S08H18A  BAS   RE,SND02H           08 HEADER, MIMMICK 02 HDR                    
*                                                                               
         USING AUTKEY,R6                                                        
S08H19   L     R6,AIO                                                           
         MVC   KEY,SVAUKEY                                                      
         CLI   AUTKREV,X'99'             ORIGINAL ONE DOESN'T HAVE MKTS         
         BE    S08H60                                                           
         LA    R6,AUTEL                  FIRST ELEMENT                          
         USING AMKTELD,R6                                                       
*                                                                               
         TM    SVINFLAG,AINFRVAL         THIS REV EDITS ALL MARKETS?            
         BNO   S08H20                    NO                                     
         LA    R1,MCMKT                  MARKET EDITTED = ALL                   
         MVI   BYTE,0                                                           
         LA    R4,BYTE                                                          
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
*                                                                               
* GET ELEMENT WITH LIST OF MARKETS REVISED                                      
S08H20   MVI   ELCDLO,AMKTAELQ           LOWEST MARKET ELEMENT CODE             
         MVI   ELCDHI,AMKTRELQ           HIGHEST MARKET ELEMENT CODE            
S08H30   BRAS  RE,NEXTEL3                                                       
         BNE   S08H60                                                           
         LR    R3,R6                     SAVE ADDRESS OF BEG OF EL              
         ZIC   R2,AMKTLEN                LENGTH OF ELEMENT                      
         SHI   R2,2                      R2 = # OF BYTES LEFT IN MKT            
S08H40   LA    R4,AMKTMKT                 LIST ELEMENT TO PROCESS               
         SR    R5,R5                     CLEAR OVERRIDE LENGTH                  
*                                                                               
* GIVE R1 VALUE OF MAPCODE BASED ON MKT CANCELD, DELETED, ADDED, EDITED         
         LA    R1,MCMKTCA                MARKET CANCELLED                       
         TM    SVINFLAG,AINFRVCN         THIS REV CANCELS MARKETS?              
         BO    S08H50                    YES                                    
         LA    R1,MCMKTDL                MARKET DELETED                         
         TM    SVINFLAG,AINFRVDL         THIS REV DELETES MARKETS?              
         BO    S08H50                    YES                                    
         LA    R1,MCMKTAD                MARKET ADDED                           
         CLI   0(R3),AMKTAELQ            ELEMENT CODE FOR ADDED MKTS            
         BE    S08H50                                                           
         LA    R1,MCMKT                  MARKET EDITTED                         
S08H50   BAS   RE,SENDD                  SEND MARKET                            
*                                                                               
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         BCT   R2,S08H40                 PROCESS NEXT MARKET                    
         LR    R6,R3                     SAVE ADDRESS OF BEG OF EL              
         B     S08H30                    CHECK FOR MORE ELEMENTS                
*                                                                               
S08H60   DS    0H                                                               
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         B     S08H10                                                           
*                                                                               
S08HX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND COMMENTS AND ATTACHMENTS                                                 
* ON ENTRY: AIO1 HAS AUTHORIZATION RECORD                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
SND0BH   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*SND0BH*'                                                    
*                                                                               
         MVI   SEND0B,C'N'                                                      
         CLC   VERSION,=X'01010020'      VERSION 1.1.0.32 & HIGH ONLY           
         BL    S0BH10                                                           
         CLI   SVRCVEL+1,X'06'           BULK AUTHORIZATION DOWNLOAD            
         BE    SND0BHX                                                          
         CLI   SVRCVEL+1,X'08'           REVISION HISTORY NEVER SENDS           
         BE    S0BH10                    0B HEADER, ONLY COMMENTS               
         MVI   SEND0B,C'Y'                                                      
*                                                                               
S0BH10   L     R6,AIO1                                                          
         MVI   ELCODE,ACOMELQ                                                   
         BRAS  RE,GETEL2                 IS THERE A COMMENT?                    
         BNE   S0BH30                    NO                                     
*                                                                               
         CLI   SEND0B,C'Y'                                                      
         BNE   S0BH20                                                           
         LA    R1,X'000B'                SEND 0B HEADER                         
         BAS   RE,SENDH                                                         
         MVI   SEND0B,C'N'                                                      
*                                                                               
         USING ACOMELD,R6                                                       
S0BH20   ZIC   R5,ACOMLEN                                                       
         AHI   R5,-3                                                            
         LA    R1,MCATHCM                AUTH COMMENT                           
         LA    R4,ACOMTXT                                                       
         BAS   RE,SENDD                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    S0BH20                                                           
*                                                                               
S0BH30   L     R6,AIO1                                                          
         MVI   ELCODE,ATCHELQ                                                   
         BRAS  RE,GETEL2                 ANY ATTACHMENTS?                       
         BNE   SND0BHX                                                          
*                                                                               
         CLI   SEND0B,C'Y'                                                      
         BNE   S0BH40                                                           
         LA    R1,X'000B'                SEND 0B HEADER                         
         BAS   RE,SENDH                                                         
*                                                                               
         USING ATCHELD,R6                                                       
S0BH40   ZIC   R5,ATCHLEN                                                       
         SHI   R5,2                                                             
*                                                                               
         CLC   VERSION,=X'02000000'      AFTER VERSION 2.0.0.0                  
         BH    S0BH80                    SEND WHOLE ELEM                        
*                                                                               
         SR    R1,R1                     OLD VERSIONS                           
         LA    R4,ATCHFLNM               JUST GET FILENAME                      
S0BH50   CLI   0(R4),C'|'                                                       
         BE    S0BH60                                                           
         LA    R1,1(R1)                                                         
         CR    R1,R5               MAX LENGTH REACHED                           
         BE    S0BH80              THEN NO | THERE - JUST SEND                  
         LA    R4,1(R4)                                                         
         B     S0BH50                                                           
*                                                                               
S0BH60   LR    R5,R1               SET SHORT LENGTH                             
*                                                                               
S0BH80   LA    R1,MCATACH          ATTACMENT MAPCODE                            
         LA    R4,ATCHFLNM         ATTACHMENT NAME                              
         BAS   RE,SENDD                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    S0BH40                                                           
SND0BHX  XIT1                                                                   
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND UDEF VALUES                                                              
* ON ENTRY: AIO1 HAS AUTHORIZATION RECORD                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
SNDUDEFS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   VERSION,=X'02000028'      VERSION 2.0.0.40 & HIGH ONLY           
         BL    SUX                                                              
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,UDEFELQ                                                   
         BRAS  RE,GETEL2                 ANY UDEFS?                             
         BNE   SUX                       NO                                     
*                                                                               
         USING UDEFELD,R6          SEND NUMBER|VALUE                            
SU10     XC    BLOCK(100),BLOCK                                                 
         MVC   BLOCK(1),UDEFSEQ    UDEF NUMBER CHAR 1-5                         
         MVI   BLOCK+1,C'|'                                                     
         ZIC   R1,UDEFLEN                                                       
         AHI   R1,-4               FOR EX                                       
         EX    R1,*+4                                                           
         MVC   BLOCK+2(0),UDEFTXT  UDEF VALUE                                   
*                                                                               
         ZIC   R5,UDEFLEN          ELEM LENGTH = DATA LENGTH +1                 
         AHI   R5,-1               SO -1 FOR SEND LENGTH                        
         LA    R4,BLOCK                                                         
         LA    R1,MCSUVAL                                                       
         BAS   RE,SENDD                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    SU10                                                             
*                                                                               
SUX      XIT1                                                                   
         DROP  R6                                                               
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CHECK IF CLIENT IS VALID FOR THIS BUYER/SUPERVISOR                            
* BCLT HAS CLIENT                                                               
* LASTCLT HAS THE LAST CLIENT PROCESSED                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
CHKCLT   NTR1  BASE=*                                                           
         CLC   LASTCLT,BCLT              SAME AS LAST CLIENT?                   
         BNE   CHKCLT05                  NO, CHECK IF VALID                     
         TM    DWNFLAG,MYCLIENT          YES, IS IT VALID                       
         BO    CHKCLEQX                                                         
         TM    DWNFLAG,CLTLOCK           NOT VALID                              
         BO    CHKCLNEX                                                         
         B     CHKCLT40                                                         
*                                                                               
CHKCLT05 NI    DWNFLAG,X'FF'-MYCLIENT-CLTLOCK                                   
         MVC   LASTCLT,BCLT                                                     
         XC    LASTPRD(3),LASTPRD        IF DIFF CLT CLEAR PRDS & EST           
         XC    QPRD2,QPRD2                                                      
*                                                                               
         TM    DWNFLAG,CHKLOCK           DO WE NEED TO CHECK SECURITY?          
         BNO   CHKCLT07                    YES, BECAUSE HAVEN'T YET             
         NI    DWNFLAG,X'FF'-CHKLOCK                                            
         GOTO1 VALICLT                                                          
         BE    CHKCLT07                                                         
         OI    DWNFLAG,CLTLOCK           INVALID                                
         B     CHKCLNEX                                                         
*                                                                               
CHKCLT07 CLI   SVUSRTP,C'S'              CHECK IF A SUPERVISOR                  
         BE    CHKCLT10                                                         
         CLI   SVBYRFLT,0                NO, CHECK FOR CLIENT FILTER            
         BE    CHKCLEQX                  NO, BUYER SEES ALL CLIENTS             
*                                                                               
         USING CLTLSTD,R2                                                       
CHKCLT10 L     R2,ACLTLST                                                       
CHKCLT20 OC    CLTCODE,CLTCODE                                                  
         BZ    CHKCLT40                                                         
         L     R3,ACLTLSTX                                                      
         CR    R2,R3                                                            
         BNL   CHKCLT40                                                         
*                                                                               
         CLC   CLTCODE,BCLT                                                     
         BNE   CHKCLT30                                                         
*                                                                               
         MVC   BYTE,CLTFLAG              TURN OFF BITS IN 1ST NIBBLE            
         NI    BYTE,X'FF'-X'F0'          TO COMPARE MEDIA CODE ONLY             
         MVC   BYTE2,BAGYMD                                                     
         NI    BYTE2,X'FF'-X'F0'                                                
*                                                                               
         CLC   BYTE,BYTE2                SAME MEDIA?                            
         BNE   CHKCLT30                                                         
         OI    DWNFLAG,MYCLIENT          CLIENT IS "MY CLIENT"                  
         B     CHKCLEQX                                                         
*                                                                               
CHKCLT30 LA    R2,CLTLSTL(R2)                                                   
         B     CHKCLT20                                                         
*                                                                               
* BRANCHES HERE IF NOT A "MY CLIENT"                                            
CHKCLT40 CLI   SVUSRTP,C'S'              SUPERVISOR?                            
         BE    CHKCLT50                                                         
         CLI   SVBYRFLT,0                NO, CHECK FOR CLIENT FILTER            
         BE    CHKCLEQX                  NO, SHOW ALL CLIENTS                   
         BNE   CHKCLNEX                  NO, BUYER W/ CLT FILTER-FAILED         
CHKCLT50 CLI   SVTRANT,MYCLNT            "BUYS FOR MY CLIENTS" DWNLD?           
         BE    CHKCLNEX                  YES, THEN SKIP THIS CLIENT             
         B     CHKCLEQX                                                         
CHKCLEQX SR    RC,RC                                                            
CHKCLNEX LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    SDBIT                                                      
* TURNS ON SUPERDESK FLAG IN ESTIMATE RECORDS DEPENDING ON CLIENT TYPE          
*    FOR TRUE POL CLIENTS, TURN ON BIT FOR ALL PRODUCT'S EST RECORD             
*    FOR BRAND AND BRAND POL, JUST THE PRD1, PRD2, & POL'S EST REC              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SDBIT    NTR1  BASE=*,LABEL=*                                                   
         CLI   SVTRANT,ADAUTH            UPDATE SD BIT ONLY ON ADDS             
         BNE   SDBITX                                                           
*                                                                               
         CLI   SVPOOL,C'T'               IF TRUE POL, CHECK CLT RECORD          
         BNE   SBIT20                      TO PROCESS ALL OTHER PRDS            
         USING CLTHDRD,R6                                                       
         L     R6,ACLTREC                                                       
         LA    R2,CLIST                                                         
*                                                                               
SBIT10   CLI   0(R2),0                                                          
         BE    SDBITX                                                           
         MVC   QPRD,0(R2)                                                       
         OI    FLAG1,PRD1+SDEBIT                                                
         GOTO1 VALIEST                                                          
         LA    R2,4(R2)                                                         
         B     SBIT10                                                           
         DROP  R6                                                               
*                                                                               
SBIT20   OI    FLAG1,PRD1+SDEBIT         USE PRD1 + TURN ON SD BIT              
         GOTO1 VALIEST                   (THIS CLEARS FLAG1)                    
         OC    QPRD2,QPRD2                                                      
         BZ    SBIT30                                                           
         OI    FLAG1,PRD2+SDEBIT         USE PRD2 + TURN ON SD BIT              
         GOTO1 VALIEST                                                          
*                                                                               
SBIT30   MVC   QPRD,=C'POL'              TURN ON FLAG FOR POL                   
         OI    FLAG1,PRD1+SDEBIT         USE PRD1 + TURN ON SD BIT              
         GOTO1 VALIEST                                                          
*                                                                               
SDBITX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* INTERFACE TO STAPACK TO GET STATION CALL LETTERS                              
*==============================================================*                
         DS    0H                                                               
STAUNPK  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*STAUNPK'                                                    
         MVI   XSP,C'N'                                                         
         XC    WORK2,WORK2                                                      
         LA    R4,WORK2                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMKST,BMKTSTA    MOVE MARKET/STATION                          
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(5),STAPQSTA                                                 
         MVC   WORK+5(3),STAPQNET                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CHECK IF MARKET IS VALID FOR THIS BUYER/SUPERVISOR                            
* IF THE USER IS A CLIENT SUPERVISOR AND THIS IS THEIR CLIENT, THEN             
* WE DON'T NEED TO CHECK THE MARKET                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
CHKMKT   NTR1  BASE=*                                                           
         CLI   SVUSRTP,C'S'              IS THIS A SUPERVISOR?                  
         BNE   CMKT10                    NO, SO ALWAYS CHECK MARKET             
         TM    DWNFLAG,MYCLIENT          IS CLIENT "MY CLIENT"?                 
         BO    EQXIT                     YES, DON'T NEED TO CHECK MKT           
*                                                                               
CMKT10   CLI   QMED,C'T'                                                        
         BNE   CMKT20                                                           
         TM    FLAG2,ALLMKTT             CHECK IF SEES ALL MKTS IN MED          
         BO    EQXIT                                                            
         L     R2,AMKTLSTT               USE T MARKET TABLE                     
CMKT20   CLI   QMED,C'R'                                                        
         BNE   CMKT30                                                           
         TM    FLAG2,ALLMKTR                                                    
         BO    EQXIT                                                            
         L     R2,AMKTLSTR               USE R MARKET TABLE                     
CMKT30   CLI   QMED,C'X'                                                        
         BNE   CMKT40                                                           
         TM    FLAG2,ALLMKTX                                                    
         BO    EQXIT                                                            
         L     R2,AMKTLSTX               USE X MARKET TABLE                     
*                                                                               
CMKT40   SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BMKT                                                        
         D     R0,=F'8'                  DIVISOR                                
         AR    R2,R1                     ADD QUOTIEND TO A(TABLE)               
         STC   R0,FULL                   REMAINDER IN 1ST BYTE OF FULL          
*                                                                               
         USING BITTABD,R3                                                       
         LA    R3,BITTAB                                                        
CMKT50   CLC   FULL(1),BITREM            MATCH REMAINDER IN TABLE               
         BE    CMKT60                    FIND OUT WHICH BIT TO TEST             
         LA    R3,BITLNQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   CMKT50                                                           
         DC    H'00'                     END OF TABLE                           
*                                                                               
CMKT60   ZIC   R1,BITBIT                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0                   TEST BIT                               
         BNO   NEXIT                                                            
         B     EQXIT                                                            
*                                                                               
*        REMAINDER/BIT TURNED ON                                                
BITTAB   DC    X'00',X'80'                                                      
         DC    X'01',X'40'                                                      
         DC    X'02',X'20'                                                      
         DC    X'03',X'10'                                                      
         DC    X'04',X'08'                                                      
         DC    X'05',X'04'                                                      
         DC    X'06',X'02'                                                      
         DC    X'07',X'01'                                                      
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                              ADDPASS                                          
* IF THE START DATE CHANGED ON AN EDIT (NOT A REVISION) DELETE THE OLD          
*    PASSIVE KEY AND ADD A NEW ONE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
ADDPASS  NTR1  BASE=*                                                           
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                   DELETE OLD '0DC9' PASSIVE KEY          
*                                                                               
         MVC   AUSKTYP(2),=X'0DC9'                                              
         MVC   AUSKAM,BAGYMD             AGENCY/MEDIA                           
         MVC   AUSKSTDT,OLDSTADT         OLD START DATE                         
         MVC   AUSKCLT,BCLT              CLT                                    
         MVC   AUSKPRD,BPRD              PRD                                    
         MVC   AUSKPRD2,BPRD2            PB PRD                                 
         MVC   AUSKEST,BEST              EST                                    
         MVC   AUSKAUN,SVVERNM           VERSION                                
         MVC   AUSKREV,SVREVNM           REVISION                               
         MVC   AUTKDA,DMDSKADD           PUT DISK ADDRESS IN KEY                
         MVC   WORK(L'KEY),KEY           SAVE FOR '0DC9' PASSIVE KEY            
         OI    AUTKSTAT,AUTKSDEL         MARK DELETED                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   KEY,WORK                  ADD '0DC9' PASSIVE KEY                 
         MVC   AUSKSTDT,SVSTADT          NEW START DATE                         
         OI    DMINBTS,X'08'             READHI FOR DELETED RECS                
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'       DMINBTS AS IT WAS                      
         CLC   KEY(L'AUPKEY),KEYSAVE     ALREADY DELETED?                       
         BNE   APAS05                    NO, ADD A NEW ONE                      
         NI    AUTKSTAT,X'FF'-AUTKSDEL   REMOVE DELETED FLAG                    
         GOTO1 WRITE                     RESTORE PASSIVE KEY                    
         B     ADDPASSX                                                         
*                                                                               
APAS05   MVC   KEY,WORK                  ADD '0DC9' PASSIVE KEY                 
         MVC   AUSKSTDT,SVSTADT          NEW START DATE                         
         NI    AUTKSTAT,X'FF'-AUTKSDEL   DON'T ADD A DELETED REC                
         GOTO1 ADD                       ADD PASSIVE KEY                        
*                                                                               
ADDPASSX XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                              ADDPASSD                                         
* IF THE DUE DATE CHANGED ON AN EDIT (NOT A REVISION) DELETE THE OLD            
*    PASSIVE KEY AND ADD A NEW ONE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
ADDPASSD NTR1  BASE=*                                                           
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                   DELETE OLD '0DB9' PASSIVE KEY          
*                                                                               
         MVC   AUSKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD             AGENCY/MEDIA                           
         MVC   AUPKDUE,OLDDUEDT          OLD DUE DATE                           
         MVC   AUPKCLT,BCLT              CLT                                    
         MVC   AUPKPRD,BPRD              PRD                                    
         MVC   AUPKPRD2,BPRD2            PB PRD                                 
         MVC   AUPKEST,BEST              EST                                    
         MVC   AUPKAUN,SVVERNM           VERSION                                
         MVC   AUPKREV,SVREVNM           REVISION                               
         MVC   AUTKDA,DMDSKADD           PUT DISK ADDRESS IN KEY                
         MVC   WORK(L'KEY),KEY           SAVE FOR '0DC9' PASSIVE KEY            
         OI    AUTKSTAT,AUTKSDEL         MARK DELETED                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   KEY,WORK                  ADD '0DB9' PASSIVE KEY                 
         MVC   AUPKDUE,SVDUEDT           NEW DUE DATE                           
         OI    DMINBTS,X'08'             READHI FOR DELETED RECS                
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'       DMINBTS AS IT WAS                      
         CLC   KEY(L'AUPKEY),KEYSAVE     ALREADY DELETED?                       
         BNE   APASD05                   NO, ADD A NEW ONE                      
         NI    AUTKSTAT,X'FF'-AUTKSDEL   REMOVE DELETED FLAG                    
         GOTO1 WRITE                     RESTORE PASSIVE KEY                    
         B     ADDPASDX                                                         
*                                                                               
APASD05  MVC   KEY,WORK                  ADD '0DB9' PASSIVE KEY                 
         MVC   AUPKDUE,SVDUEDT           NEW DUE DATE                           
         NI    AUTKSTAT,X'FF'-AUTKSDEL   DON'T ADD A DELETED REC                
         GOTO1 ADD                       ADD PASSIVE KEY                        
ADDPASDX XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CHECK GOALS FOR NEW MARKETS                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         DS    0H                                                               
CHKGOA   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*CHKGOA*'                                                    
*                                                                               
         CLI   SVTRANT,EDTAUTH           IF NOT EDITING, ADDING                 
         BNE   CHKG10                    SO MUST CHECK GOALS FOR MKTS           
         TM    OVFLAG1,FLTDTCH           DID DATES CHANGE?                      
         BNO   CHKGOAX                   NO, THEN DON'T CHECK GOALS             
*                                                                               
CHKG10   OI    OVFLAG1,GOALMKT           MARKETS FOUND THROUGH GOALS            
         L     R1,AIO4                                                          
         MVC   0(2,R1),=X'FFFF'          INITIALIZE TABLE OF MKTS               
         USING GOLRECD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
         CLI   BPRD,X'FF'                                                       
         BE    *+10                                                             
         MVC   GKEYPRD,BPRD                                                     
*                                                                               
CHKG20   MVI   XSP,C'N'                                                         
         GOTO1 HIGH                                                             
*                                                                               
CHKG30   CLC   KEY(5),KEYSAVE            TYPE,A/M,CLT,PRD                       
         BE    CHKG40                                                           
         CLI   SVPOOL,C'T'               TEST TRUE POL                          
         BNE   CHKG100                   NO MORE GOALS                          
         CLC   KEY(4),KEYSAVE            TYPE,A/M/CLT                           
         BNE   CHKG100                   NO MORE GOALS                          
         CLI   KEY+4,X'FF'               PRD POL IS CPP GUIDE                   
         BE    CHKG100                                                          
*                                                                               
CHKG40   CLC   GKEYEST,BEST              SAME EST?                              
         BE    CHKG60                                                           
         BL    CHKG50                    IF LOW, READ FOR ESTIMATE              
         MVC   GKEYEST(6),=6X'FF'        FORCE NEW MARKET                       
         B     CHKG20                                                           
*                                                                               
CHKG50   MVC   GKEYEST,BEST              READ FOR ESTIMATE                      
         XC    GKEYDPT(5),GKEYDPT        CLEAR REST OF KEY                      
         B     CHKG20                                                           
*                                                                               
CHKG60   CLI   BPRD2,0                   IS THERE A PIGGYBACK?                  
         BE    CHKG70                    NO                                     
         TM    GKEYAGY,X'40'             IS GKEYPRD2 A PRODUCT?                 
         BO    CHKG90                    NO, CHECK NEXT RECORD                  
         CLC   GKEYPRD2,BPRD2            CORRECT PIGGYBACK?                     
         BNE   CHKG90                                                           
*                                                                               
CHKG70   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKG90                                                           
         USING GLEMENT,R6                                                       
         MVC   WORK2(2),GLWEEK           START DATE                             
CHKG80   MVC   WORK2+2(2),GLWEEK         END DATE                               
         BAS   RE,NEXTEL                                                        
         BE    CHKG80                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(2,WORK2),(3,WORK)    START DATE IN PACKED          
         GOTO1 (RF),DMCB,(2,WORK2+2),WORK2+4      END WEEK IN CHAR              
         GOTO1 VADDAY,DMCB,(C'D',WORK2+4),WORK2+10,F'6' END OF WEEK             
         GOTO1 VDATCON,DMCB,WORK2+10,(3,WORK+3)    END DATE IN PACKED           
                                                                                
*                                                                               
         CLC   SVSTADT,WORK+3            START DATE > GOAL END DATE?            
         BH    CHKG90                    YES, SO NOT IN PERIOD                  
         CLC   SVENDDT,WORK              END DATE < GOAL START DATE             
         BL    CHKG90                    YES, SO NOT IN PERIOD                  
         GOTO1 =A(MKTTAB),RR=OVRELO      UPDATE TABLE OF MARKETS                
*                                                                               
CHKG90   GOTO1 SEQ                                                              
         B     CHKG30                                                           
         DROP  R4                                                               
CHKG100  L     R3,AIO4                   BITS FOR ALL MARKETS                   
         LA    R2,0                      MARKET NUMBER                          
*                                                                               
         USING MKTTABD,R3                                                       
CHKG110  CLC   MKTNUM,=X'FFFF'                                                  
         BE    CHKGOAX                                                          
         ZICM  R2,MKTNUM,2               PUT MARKET NUMBER IN R2                
         MVC   FULL,MKTCRDT              SAVE DATES                             
         MVC   SVMKTDD,SVDUEDT                                                  
         GOTO1 =A(MKTCHK),RR=OVRELO      UPDATE TABLE OF MARKETS                
         LA    R3,MKTTABLQ(R3)                                                  
         B     CHKG110                                                          
         DROP  R3                                                               
CHKGOAX  XIT1                                                                   
         LTORG                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CHECK IF MARKET RECORDS EXIST, IF NOT ADD MARKET RECORD                       
* ON ENTRY, R2 HAS MARKET NUMBER                                                
*           FULL HAS GOAL DATES, IF MARKET CAME FROM GOALS                      
*           SVMKTDD HAS MKT LEVEL DUE DATE                                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
MKTCHK   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*MKTCHK*'                                                    
*                                                                               
         USING AUTRECD,R6                                                       
         MVI   BYTE,0                    CLEAR NEW MKT FLAG                     
         MVI   XSP,C'Y'                                                         
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         STCM  R2,3,AUTKMKT                                                     
         MVI   AUTKREV,0                 NO REV # IN AUTH MKT REC KEY           
         MVC   KEYSAVE,KEY               IN CASE ADDING                         
         CLI   SVTRANT,EDTAUTH           IF NOT EDIT, MKT DOESN'T EXIST         
         BNE   MKTCHK10                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(AUTKSTA-AUTRECD),KEYSAVE                                     
         BNE   MKTCHK10                                                         
         GOTO1 =A(REVMKT),RR=OVRELO                                             
         B     MKTCHKX                                                          
*                                                                               
MKTCHK10 MVI   BYTE,1                    SET FLAG FOR NEW MARKET                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         LR    R0,R6                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         USING AUTRECD,R6                                                       
         MVC   AUTKEY,KEYSAVE                                                   
         MVC   AUTRAGYA,TWAAGY           ALPHA AGY                              
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R3,WORK2                                                         
         USING MINFEL,R3                                                        
         MVI   MINFEL,MINFELQ            ELEMENT CODE                           
         MVI   MINFLEN,MINFLNQ           LENGTH                                 
         MVC   MINFRVNM,SVREVNM          REV NUMBER                             
         MVC   MINFRVDT,TODAY            REV DATE                               
         MVC   MINFDUDT,SVMKTDD          MARKET DUE DATE                        
*                                                                               
         CLI   SVTRANT,EDTAUTH           IF EDIT, THEN MANUALLY ADDING          
         BNE   MKTCHK20                    SO NO GOAL DATES                     
         OI    MINFFLAG,MINFMAN          MANUALLY ADDED MARKET                  
         B     MKTCHK30                  SKIP GOAL DATES                        
*                                                                               
MKTCHK20 GOTO1 VDATCON,DMCB,(2,FULL),(3,MINFGIDT) GOAL INPUT DATE               
         GOTO1 (RF),DMCB,(2,FULL+2),(3,MINFGCDT)  GOAL CHANGE DATE              
*                                                                               
MKTCHK30 GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R6),WORK2,=C'ADD=CODE'            
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
MKTCHKX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    READ ALL ACTIVE MARKETS                                    
* READS ALL ACTIVE MARKETS                                                      
* CALL REVMKT WHICH REVISES THEM                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RDAMKT   NTR1  BASE=*,LABEL=*                                                   
         OI    OVFLAG1,REVACMKT          REVISE ALL ACTIVE MARKETS              
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         MVI   AUTKREV,0                 NO REV # IN AUTH MKT REC KEY           
         MVI   AUTKMKT+1,1               LOOK FOR FIRST MARKET                  
         MVI   XSP,C'Y'                                                         
RDAMKT10 GOTO1 HIGH                                                             
         CLC   KEY(AUTKMKT-AUTRECD),KEYSAVE                                     
         BNE   RDAMKTX                                                          
*                                                                               
         MVC   SVMKTDD,SVDUEDT           DEFAULT TO AUTH DUE DATE               
         CLC   VERSION,=X'0201002C'      AFTER VERSION 2.1.0.44 ONLY            
         BL    RDAMKT30                                                         
*                                                                               
         TM    SVFLAG1,F1CANMK+F1DELMK   IF CANCEL OR DELETE                    
         BNZ   RDAMKT15                                                         
         TM    SVFLAG1,F1DUEDT     WAS AN AUTH LEVEL DUE DATE SENT              
         BO    *+10                IF NOT SENT DON'T CHANGE                     
         XC    SVMKTDD,SVMKTDD     CLEAR = DON'T CHANGE!                        
*                                                                               
RDAMKT15 TM    FLAG2,MKTDUES       MKT AND DUE DATE IN TABLE                    
         BNO   RDAMKT30                                                         
         XC    SVMKTDD,SVMKTDD     CLEAR = DON'T CHANGE!                        
*                                                                               
         L     R3,AIO4                   SELECTED MKTS                          
RDAMKT20 CLC   =X'FFFF',0(R3)            END OF MARKET LIST?                    
         BE    RDAMKT30                  YES                                    
         CLC   AUTKMKT,0(R3)       MATCH ON MKT                                 
         BNE   *+14                                                             
         MVC   SVMKTDD,2(R3)       SPECIAL MKT DUE DATE                         
         B     RDAMKT30                                                         
         LA    R3,5(R3)                                                         
         B     RDAMKT20                                                         
*                                                                               
RDAMKT30 BRAS  RE,REVMKT                                                        
         MVC   AUTKSTA(4),=X'FFFFFFFF'                                          
         B     RDAMKT10                                                         
*                                                                               
RDAMKTX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    REVISE MARKET RECORD                                       
* UPDATE DUE DATE, REVISION NUMBER AND REVISION DATE                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
REVMKT   NTR1  BASE=*,LABEL=*                                                   
         MVI   OVMFLAG1,0                                                       
         USING AUTRECD,R6                                                       
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         TM    OVFLAG1,GOALMKT           MARKET CHNG BECAUSE EXPANDING          
         BNO   REVMKT04                  FLIGHT DATE & FOUND NEW GOALS?         
         GOTO1 VDATCON,DMCB,(2,FULL),(3,MINFGIDT) GOAL INPUT DATE               
         GOTO1 (RF),DMCB,(2,FULL+2),(3,MINFGCDT)  GOAL CHANGE DATE              
         GOTO1 PUTREC                                                           
         B     REVMKTX                                                          
*                                                                               
REVMKT04 TM    AUTRSTAT,AUTRSCAN         MARKET CANCELLED?                      
         BNO   *+8                                                              
         OI    OVMFLAG1,MKTCANLD         YES, MARKET IS CANCELLED               
         TM    AUTRSTAT,AUTRSDL          MARKET "DELETED"                       
         BNO   *+8                                                              
         OI    OVMFLAG1,MKTDELD          YES, MARKET IS DELETED                 
*                                                                               
         TM    OVMFLAG1,MKTCANLD+MKTDELD                                        
         BZ    REVMKT10                                                         
         LA    R1,ADDMKTS          CHECK TABLE TO RE-ADD MKT                    
         LA    R0,20                                                            
REVMKT05 OC    0(2,R1),0(R1)       NO ENTRY = END                               
         BZ    REVMKT08            THEN DON'T CHANGE THIS MARKET                
         CLC   AUTKMKT,0(R1)       MARKET IN TABLE                              
         BE    REVMKT10            GO RE-ADD                                    
         LA    R1,2(R1)                                                         
         BCT   R0,REVMKT05                                                      
REVMKT08 OI    OVMFLAG1,NOREADD    DO NOT RE-ADD MKT                            
*                                                                               
REVMKT10 MVC   MINFRVNM,SVREVNM          REVISION NUMBER                        
         MVC   MINFRVDT,TODAY            REVISION DATE                          
*                                                                               
         NI    OVFLAG1,X'FF'-MKTDUECH                                           
         OC    SVMKTDD,SVMKTDD           IF NO MKT DUE DATE                     
         BZ    REVMKT11                  THEN NO CHANGE                         
         CLC   MINFDUDT,SVMKTDD          DID MKT DUE DATE CHANGE?               
         BE    REVMKT11                                                         
         MVC   MINFDUDT,SVMKTDD          MARKET DUE DATE                        
*                                                                               
         TM    OVMFLAG1,NOREADD          DO NOT RE-ADD MKT -                    
         BO    *+8                       JUST CHANGE DUE DATE                   
         OI    MINFFLAG,MINFREV          REV/EDIT LATEST STATUS                 
*                                                                               
         OI    OVFLAG1,MKTDUECH          SET MKT DUE DATE CHANGED               
*                                                                               
REVMKT11 MVC   SVMKTREV,MINFRVNO         MKT REV# FOR SAVED APP HIST            
         TM    OVFLAG1,NEWREV            CHECK IF ADDING NEW REVISION           
         BNO   REVMKT20                                                         
         ZIC   R1,MINFRVNO               INCREMENT # OF TIMES MKT REVSD         
         AHI   R1,1                                                             
         STC   R1,MINFRVNO                                                      
*                                                                               
*   SPECIAL PROFILE TO RESET STATUS ON EVERY REVISION AND ADD TO COUNTS         
*                                                                               
         CLI   SVSDPRF+2,C'Y'            RESET STATUS ON REVISION               
         BNE   REVMKT20                                                         
*                                                                               
         MVI   SVAPP,0                                                          
         MVI   SVCOMP,0                                                         
         MVI   SVREJ,0                                                          
*                                                                               
         LA    R6,AUTEL                 FIRST ELEMENT                           
REVMKT12 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                  NO ELEMENTS?                            
         BE    REVMKT14                                                         
         CLI   0(R6),MSTAREJQ           LOWEST STATUS ELEMENT CODE              
         BL    REVMKT12                                                         
         CLI   0(R6),MSTACMSQ           HIGHEST STATUS ELEMENT CODE             
         BH    REVMKT12                                                         
         USING MSTAELD,R6                                                       
         CLI   0(R6),MSTAREJQ           REJECTED                                
         BNE   *+8                                                              
         MVI   SVREJ,1                                                          
         CLI   0(R6),MSTAAPPQ           APPROVED                                
         BNE   *+8                                                              
         MVI   SVAPP,1                                                          
         CLI   0(R6),MSTACMPQ           BUYER COMPLETED                         
         BNE   *+8                                                              
         MVI   SVCOMP,1                                                         
         CLI   0(R6),MSTACMSQ           SUPV COMPLETED                          
         BNE   *+8                                                              
         MVI   SVCOMP,1                                                         
         B     REVMKT12                                                         
*                                                                               
REVMKT14 OC    SVSTAT,SVSTAT             ANY STATUS TO RECORD?                  
         BZ    REVMKT20                  NO THEN DONE                           
*                                                                               
         TM    OVMFLAG1,MKTCANLD+MKTDELD   IF MKT IS ALREADY CANX/DEL           
         BNZ   REVMKT20                    THEN DON'T CHANGE STATUS             
*                                                                               
         BRAS  RE,SAVEHIST               SAVE APPROVAL HISTORY                  
         MVI   ELCDLO,MSTAREJQ           DELETE ALL STATUS ELEMENTS             
         MVI   ELCDHI,MSTACMPQ           EXCEPT FOR CANCEL/DELETED              
         BRAS  RE,DELEL                                                         
         MVI   ELCDLO,MSTACMSQ           DELETE SUPV COMP                       
         MVI   ELCDHI,MSTACMSQ                                                  
         BRAS  RE,DELEL                                                         
*                                                                               
         L     R6,AIO                                                           
         USING AUTRECD,R6                                                       
         LA    R6,AUTEL                 FIRST ELEMENT                           
         USING MSCTELD,R6                                                       
REVMKT16 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                  NO STATUS COUNT ELEM?                   
         BE    REVMKT18                                                         
         CLI   0(R6),MSCTELQ            X'35'                                   
         BNE   REVMKT16                                                         
*                                                                               
         SR    R1,R1                                                            
         CLI   SVAPP,0                                                          
         BE    *+16                                                             
         IC    R1,MSCTAPP                                                       
         LA    R1,1(R1)                                                         
         STC   R1,MSCTAPP                                                       
*                                                                               
         CLI   SVREJ,0                                                          
         BE    *+16                                                             
         IC    R1,MSCTREJ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,MSCTREJ                                                       
*                                                                               
         CLI   SVCOMP,0                                                         
         BE    *+16                                                             
         IC    R1,MSCTCOMP                                                      
         LA    R1,1(R1)                                                         
         STC   R1,MSCTCOMP                                                      
*                                                                               
         B     REVMKT20                                                         
         DROP  R6                                                               
*                                                                               
REVMKT18 XC    BLOCK,BLOCK               ADD STATUS COUNT ELEM                  
         LA    R3,BLOCK                                                         
         USING MSCTELD,R3                                                       
         MVI   MSCTEL,MSCTELQ            X'35'                                  
         MVI   MSCTLEN,MSCTLENQ          ELEMENT LENGTH                         
         MVC   MSCTAPP,SVAPP                                                    
         MVC   MSCTCOMP,SVCOMP                                                  
         MVC   MSCTREJ,SVREJ                                                    
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO,BLOCK,0                        
         DROP  R3                                                               
*                                                                               
* CHECK IF TRYING TO ADD A CANCELLED MARKET                                     
* IF SO, DELETE ALL STATUS ELEMENTS                                             
         USING AUTRECD,R6                                                       
REVMKT20 L     R6,AIO                                                           
         TM    OVMFLAG1,NOREADD          DO NOT RE-ADD MKT                      
         BO    REVMKT60                  AND JUST CHANGE DUE DATE               
*                                                                               
         TM    OVMFLAG1,MKTCANLD+MKTDELD  MARKET CANCELLED/DELETED ?            
         BZ    REVMKT50                  NO                                     
         TM    OVFLAG1,REVACMKT          ONLY REVISING ACTIVE MARKETS?          
         BNO   REVMKT30                  NO, THEN ADDING THIS MKT BACK          
         TM    OVFLAG1,MKTDUECH          DID MKT DUE DATE CHANGE?               
         BNO   REVMKTX                   NO, SO NOT CHNGING THIS MKT            
         NI    MINFFLAG,X'FF'-MINFREV    KEEP CANCEL AS LATEST STATUS           
         B     REVMKT60                  AND JUST CHANGE DUE DATE               
REVMKT30 MVC   ERROR,=Y(MKTCNLD)                                                
         TM    SVFLAG1,F1CANMK+F1DELMK   TRYING CANCEL/DELETE MARKET?           
         BZ    REVMKT40                                                         
****>                                                                           
****>                                                                           
******>  DC    H'0'                DUMP TO FIND BUG!  REMOVE FOR LIVE           
****>                                                                           
****>                                                                           
         MVI   ERRORFLG,C'A'             UNDO FILE UPDATE FOR THIS TRAN         
         J     SNDERMSG                  TRYING TO CANCEL CANCLD MKT            
*                                                                               
REVMKT40 TM    OVMFLAG1,MKTCANLD         RE-ADDING A CANCELED MKT               
         BNO   *+12                                                             
         OI    OVMFLAG1,ADDCNLD                                                 
         NI    AUTRSTAT,X'FF'-AUTRSCAN   TURN OFF CANCEL STATUS                 
*                                                                               
         TM    OVMFLAG1,MKTDELD          RE-ADDING A DELETED MKT                
         BNO   *+12                                                             
         OI    OVMFLAG1,ADDDELD                                                 
         NI    AUTRSTAT,X'FF'-AUTRSDL    TURN OFF DELETE STATUS                 
*                                                                               
         BRAS  RE,SAVEHIST               SAVE APPROVAL HISTORY                  
         MVI   ELCDLO,MSTAREJQ                                                  
         MVI   ELCDHI,MSTADELQ                                                  
         BRAS  RE,DELEL                  DELETE ALL STATUS ELEMENTS             
         MVI   BYTE,1                    SET FLAG FOR NEW MARKET                
         B     REVMKT60                                                         
*                                                                               
* IF CANCELLING MARKET, ADD CANCEL STATUS ELEMENT TO APPROVAL HISTORY           
* DELETE ALL OTHER STATUS ELEMENTS.                                             
*                                                                               
REVMKT50 TM    SVFLAG1,F1CANMK+F1DELMK   CANCEL/DELETE MARKETS?                 
         BZ    REVMKT60                                                         
         NI    MINFFLAG,X'FF'-MINFREV    REV NO LONGER LATEST STATUS            
*                                                                               
         TM    SVFLAG1,F1CANMK           CANCEL                                 
         BZ    *+8                                                              
         OI    AUTRSTAT,AUTRSCAN         MARKET IS CANCELLED                    
*                                                                               
         TM    SVFLAG1,F1DELMK           DELETE                                 
         BZ    *+8                                                              
         OI    AUTRSTAT,AUTRSDL          MARKET IS DELETED                      
*                                                                               
         BRAS  RE,SAVEHIST               SAVE APPROVAL HISTORY                  
         MVI   ELCDLO,MSTAREJQ           DELETE ALL STATUS ELEMENTS             
         MVI   ELCDHI,MSTACMPQ           EXCEPT FOR CANCELLED                   
         BRAS  RE,DELEL                                                         
         MVI   ELCDLO,MSTACMSQ           DELETE ALL STATUS ELEMENTS             
         MVI   ELCDHI,MSTACMSQ           EXCEPT FOR CANCELLED/DELETED           
         BRAS  RE,DELEL                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING MSTAELD,R3                                                       
         MVI   MSTAEL,MSTACANQ           ELEMENT CODE                           
         TM    SVFLAG1,F1DELMK           DELETE                                 
         BZ    *+8                                                              
         MVI   MSTAEL,MSTADELQ           ELEMENT CODE                           
         MVI   MSTALEN,MSTALENQ          ELEMENT LENGTH                         
         MVC   MSTADATE,TODAY            STATUS DATE                            
         MVC   MSTABYGR,SVBYGRP          BUYING GROUP                           
         MVC   MSTACODE,SVUSRCD          USE CODE                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO,BLOCK,0                        
         DROP  R3                                                               
REVMKT60 GOTO1 PUTREC                                                           
*                                                                               
         TM    OVMFLAG1,NOREADD          DO NOT RE-ADD MKT                      
         BO    REVMKTX             SO DONE                                      
         TM    OVMFLAG1,ADDCNLD+ADDDELD  RE-ADDING A CAN/DEL MARKET             
         BNZ   REVMKT70                  YES, NEED TO TURN OFF CAN BIT          
         TM    SVFLAG1,F1CANMK+F1DELMK   CANCEL/DELETING MARKET?                
         BZ    REVMKTX                                                          
*                                                                               
REVMKT70 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         OI    AUTKSTAT,AUTKSCAN         MARKET IS CANCELLED                    
         TM    SVFLAG1,F1CANMK           CANCEL MARKETS?                        
         BO    *+8                                                              
         NI    AUTKSTAT,X'FF'-AUTKSCAN   NO, MUST BE RE-ADDING TURN OFF         
*                                                                               
         OI    AUTKSTAT,AUTKSDL          MARKET IS DELETED                      
         TM    SVFLAG1,F1DELMK           DO I WANT IT DELETED?                  
         BO    *+8                                                              
         NI    AUTKSTAT,X'FF'-AUTKSDL    NO, MUST BE RE-ADDING TURN OFF         
         GOTO1 WRITE                                                            
*                                                                               
REVMKTX  XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                    SAVE APPROVAL HISTORY                                      
*    UPDATE X'38' MARKET STATUS HISTORY ELEMENTS - MARKET REC IN AIO            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*                                                                               
SAVEHIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   VERSION,=X'02000028'      AFTER VERSION 2.0.0.40 ONLY            
         BL    SHX                                                              
*                                                                               
         MVI   ELCDLO,X'37'        DELETE 37 ELEMS - NOW USE X'38'              
         MVI   ELCDHI,X'37'                                                     
         BRAS  RE,DELEL                                                         
*                                                                               
         MVI   HISTXIST,C'N'       IS THERE A HISTORY STARTED                   
         MVI   NXTSEQ,X'FF'        START WITH SEQ NUMB FF (COMP'D)              
*                                                                               
         L     R6,AIO              MARKET LEVEL AUTH RECORD                     
         MVI   ELCODE,MSHIELQ      X'38' STATUS HISTORY                         
         USING MSHIELD,R6                                                       
         BRAS  RE,GETEL2                                                        
         BNE   SH20                NO ELEMS USE SEQ FF                          
         MVC   NXTSEQ,MSHISEQ                                                   
         ZIC   R1,NXTSEQ                                                        
         AHI   R1,-1                                                            
         STC   R1,NXTSEQ           ELSE USE FIRST ELEM SEQ-1                    
         MVI   HISTXIST,C'Y'       YES THERE IS HISTORY                         
*                                                                               
*  CHECK RECORD LENGTH - DELETE OLD HISTORY                                     
*  LEAVE ABOUT 500 BYTES FOR NEW STATUS - TRY TO LEAVE LAST 5 STATUSES          
*                                                                               
SH20     CLI   HISTXIST,C'Y'                                                    
         BNE   SH50                NO HISTORY YET - JUST GO BUILD               
         USING AUTRECD,R6                                                       
SH25     L     R6,AIO              MARKET LEVEL AUTH RECORD                     
         SR    R1,R1                                                            
         ICM   R1,3,AUTRLEN                                                     
         CH    R1,=H'3300'         IS RECORD LEN > 3300                         
         BNH   SH50                NO THEN CONTINUE                             
*                                                                               
***>     DC    H'0'                                                             
*                                                                               
         XC    SV37ADR,SV37ADR                                                  
         XC    SV37ADR2,SV37ADR2                                                
         L     R6,AIO              MARKET LEVEL AUTH RECORD                     
         LA    R6,AUTEL                  FIRST ELEMENT                          
         USING MSHIELD,R6                                                       
         MVI   ELCDLO,MSHIELQ      X'38' STATUS HISTORY                         
         MVI   ELCDHI,MSHIELQ      X'38' STATUS HISTORY                         
SH30     BRAS  RE,NEXTEL3                                                       
         BNE   SH35                                                             
         MVC   SV37ADR2,SV37ADR    2ND TO LAST 38 ELEM                          
         ST    R6,SV37ADR          LAST 38 ELEM                                 
         B     SH30                                                             
*                                                                               
SH35     ICM   R6,15,SV37ADR       LAST ELEM                                    
         BZ    SH50                                                             
         MVI   0(R6),X'FE'         SET TO DELETE                                
         CLI   MSHISTAT,MSHICMPQ   BUYER COMPLETED LAST                         
         BE    SH40                                                             
         CLI   MSHISTAT,MSHICMSQ   SUPV COMPLETED LAST                          
         BE    SH40                                                             
         B     SH45                NOT A COMPLETE=JUST DELETE 1                 
*                                                                               
SH40     ICM   R6,15,SV37ADR2      SECOND TO LAST ELEM                          
         BZ    SH45                                                             
         CLI   MSHISTAT,MSHICMPQ   IF ANOTHER COMPLETE STOP                     
         BE    SH45                                                             
         CLI   MSHISTAT,MSHICMSQ   SUPV COMPLETED LAST                          
         BE    SH45                                                             
SH42     MVI   0(R6),X'FE'         DELETE IF NOT A COMPLETE                     
*                                                                               
SH45     MVI   ELCDLO,X'FE'        DELETE FE ELEMS                              
         MVI   ELCDHI,X'FE'                                                     
         BRAS  RE,DELEL                                                         
         B     SH25                GO BACK AND SEE IF ROOM NOW                  
*                                                                               
*                                                                               
*  SAVE MARKET STATUS HISTORY IN X'38' ELEMS                                    
*                                                                               
         USING AUTRECD,R6                                                       
SH50     L     R6,AIO              MARKET LEVEL AUTH RECORD                     
* FIND FIRST STATUS ELEMENT                                                     
         LA    R6,AUTEL                  FIRST ELEMENT                          
         USING MSTAELD,R6                                                       
         MVI   ELCDLO,MSTAREJQ           LOWEST STATUS ELEMENT CODE             
         MVI   ELCDHI,MSTADELQ           HIGHEST STATUS ELEMENT CODE            
SH55     BRAS  RE,NEXTEL3                                                       
         BNE   SHX                                                              
*                                                                               
         XC    BLOCK,BLOCK               ADD STATUS HISTORY ELEM                
         LA    R3,BLOCK                                                         
         USING MSHIELD,R3                                                       
         MVI   MSHIEL,MSHIELQ            X'38'                                  
         MVI   MSHILEN,MSHILENQ          ELEMENT LENGTH - FIXED                 
         MVC   MSHIDATE,MSTADATE                                                
         XC    MSHIDATE,=X'FFFFFF' COMPLEMENT DATE                              
         MVC   MSHISEQ,NXTSEQ                                                   
         MVC   MSHISTAT,MSTAEL     ORIGINAL ELEMENT CODE                        
         MVC   MSHIBYGR,MSTABYGR   GROUP                                        
         MVC   MSHICODE,MSTACODE   USER                                         
         MVC   MSHIMREV,SVMKTREV   MKT REVISION NUMBER                          
         ZIC   R1,MSTALEN                                                       
         SH    R1,=Y(MSTALENQ+1)   PREP FOR EX OF COMMENT                       
         BM    SH60                NO COMMENT                                   
         EX    R1,*+4                                                           
         MVC   MSHICMT(0),MSTACMT                                               
         ZIC   R1,MSTALEN                                                       
         CHI   R1,252              IF REAL ELEM LEN=252 OR >                    
         BL    SH59                                                             
         LA    R1,255              THEN JUST USE MAX ELEM LEN                   
         B     *+8                                                              
SH59     AHI   R1,3                38 ELEM IS 3 BYTES BIGGER                    
         STC   R1,MSHILEN                                                       
SH60     GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO,BLOCK,0                        
         B     SH55                                                             
*                                                                               
SHX      XIT1                                                                   
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* UPDATE TABLE IN AIO4 OF MARKETS THAT HAVE GOALS                               
* ON ENTRY: AIO2 CONTAINS GOAL RECORD                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         DS    0H                                                               
MKTTAB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GOLRECD,R6                                                       
         L     R6,AIO2                   GOAL RECORD                            
         USING MKTTABD,R3                                                       
         L     R3,AIO4                   MARKET TABLE                           
MKTTAB10 CLC   MKTNUM(2),=X'FFFF'        END OF TABLE?                          
         BE    MKTTAB20                  YES, ADD MARKET TO TABLE               
         CLC   GKEYMKT(2),MKTNUM         SAME MARKET?                           
         BE    MKTTAB30                  YES, UPDATE TABLE                      
         LA    R3,MKTTABLQ(R3)                                                  
         B     MKTTAB10                                                         
*                                                                               
MKTTAB20 MVC   MKTNUM,GKEYMKT                                                   
         MVC   MKTCRDT,GREDATE           CREATION DATE                          
         MVC   MKTACDT,GACTDATE          ACTIVITY DATE                          
         MVC   MKTTABLQ(2,R3),=X'FFFF'   MARK END OF TABLE                      
         B     MKTTABX                                                          
*                                                                               
MKTTAB30 CLC   MKTCRDT,GREDATE           SAVE EARLIEST CREATION DATE            
         BNH   *+10                                                             
         MVC   MKTCRDT,GREDATE                                                  
         CLC   MKTACDT,GACTDATE          SAVE LATEST ACTIVITY DATE              
         BNL   *+10                                                             
         MVC   MKTACDT,GACTDATE                                                 
*                                                                               
         DROP  R3,R6                                                            
MKTTABX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* GET PRODUCT NAME FROM BINARY CODE                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
GETPRD   NTR1  BASE=*,LABEL=*                                                   
         TM    FLAG1,PRD2                PIGGYBACK PRODUCT?                     
         BO    GETPRD10                                                         
         CLC   LASTPRD,BPRD              SAME PRODUCT?                          
         BE    GETPRDX                                                          
         MVI   LASTEST,0                                                        
         B     GETPRD20                                                         
*                                                                               
GETPRD10 CLC   LASTPRD2,BPRD2            SAME PIGGYBACK PRODUCT?                
         BE    GETPRDX                                                          
         XC    QPRD2,QPRD2               IN CASE THERE ISN'T A PB PRD           
         XC    LASTPRD2,LASTPRD2                                                
         CLI   BPRD2,0                                                          
         BE    GETPRDX                                                          
*                                                                               
GETPRD20 L     R6,ACLTREC                                                       
         USING CLTHDRD,R6                                                       
*                                                                               
         LA    R2,CLIST                                                         
GETPRD30 CLI   0(R2),0                   END OF LIST?                           
         BNE   *+6                                                              
         DC    H'0'                      PRODUCT NOT IN LIST!                   
*                                                                               
         TM    FLAG1,PRD2                PIGGYBACK PRODUCT                      
         BO    GETPRD40                                                         
         CLC   BPRD,3(R2)                COMPARE BINARY CODE                    
         BNE   GETPRD50                                                         
         MVC   QPRD,0(R2)                                                       
         GOTO1 VALIPRD                                                          
         MVC   LASTPRD,BPRD                                                     
         B     GETPRDX                                                          
*                                                                               
GETPRD40 CLC   BPRD2,3(R2)               COMPARE BINARY CODE                    
         BE    GETPRD60                                                         
GETPRD50 LA    R2,4(R2)                                                         
         B     GETPRD30                                                         
*                                                                               
GETPRD60 MVC   QPRD2,0(R2)                                                      
         GOTO1 VALIPR2                                                          
         MVC   LASTPRD2,BPRD2                                                   
GETPRDX  MVI   FLAG1,0                                                          
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        DEMOCON                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
DEMOCONV NTR1  BASE=*                                                           
         L     R4,AIO2                                                          
         LR    R0,R4                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AIO3                                                          
         LR    R0,R5                                                            
         L     R1,=A(LENIO)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING DBLOCK,R4                 SET UP CALL TO DEMOCON                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '            SET DBFILE = NAD FOR NETWORK           
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'            SET DBSELMED = C IF CANADIAN           
         BNE   DCONV10                   AGENCY USING US DEMOS                  
         CLI   SVCXTRA,C'U'              SET DBSELMED = R OTHERWISE             
         BE    DCONV10                                                          
         MVI   DBSELMED,C'C'                                                    
DCONV10  MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 VCALLOV,DMCB              CALL DEMOCON                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(20,SVEDEMOS),(13,(R5)),(C'S',(R4)),SVEUSRNM           
         DROP  R4                                                               
*                                                                               
         GOTO1 =A(FMTDEMO),RR=OVRELO     GET TARGET DEMO                        
         XIT1                                                                   
         LTORG                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FORMAT DEMO                                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0H                                                               
FMTDEMO  NTR1  BASE=*                                                           
         L     RF,AIO3                                                          
         USING ESTHDR,R3                                                        
         MVC   WORK(11),SPACES           INITIALIZE WORK                        
         MVI   WORK,0                                                           
         CLI   0(RF),C' '                IF NO DEMO TO FORMAT ... EXIT          
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,10(RF)                                                        
FMTD5    CLI   0(R4),C' '                SCAN BACKWARDS FOR NON-SPACE           
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK                   LENGTH OF DEMO INTO WORK               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)           DEMO DESCRIPTION INTO WORK+1           
*                                                                               
         CLC   WORK+1(7),SVEWGTNM                                               
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1         IF DEMO MATCHES WEIGHTED DEMO          
         MVC   WORK+1(2),=C'W/'          INSERT HEADER                          
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK                   UPDATE LENGTH                          
         AHI   R1,2                                                             
         STC   R1,WORK                                                          
*                                                                               
FMTDEMOX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSDEWRK                                                       
         EJECT                                                                  
BITTABD  DSECT                                                                  
BITREM   DS    XL1                                                              
BITBIT   DS    XL1                                                              
BITLNQ   EQU   *-BITTABD                                                        
*                                                                               
MKTTABD  DSECT                                                                  
MKTNUM   DS    XL2                          MARKET CODE                         
MKTCRDT  DS    XL2                          MARKET CREATION DATE                
MKTACDT  DS    XL2                          MARKET ACTIVITY DATE                
MKTTABLQ EQU   *-MKTTABD                                                        
*                                                                               
TWAD     DSECT                                                                  
         ORG   TSARREC                                                          
HREC     DS    0D                                                               
HKEY     DS    0XL7                                                             
HCLT     DS    XL2                                                              
HPRD     DS    XL1                                                              
HPRD2    DS    XL1                                                              
HEST     DS    XL1                                                              
HAUN     DS    XL1                                                              
HREV     DS    XL1                                                              
HKEYX    EQU   *                                                                
HRECX    EQU   *                                                                
HRECLN   EQU   *-HREC                                                           
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
********** 480 BYTES TOTAL                                                      
*                                                                               
AMKTLSTT DS    A                         ADDRESS OF T MARKET LIST               
AMKTLSTR DS    A                         ADDRESS OF R MARKET LIST               
AMKTLSTX DS    A                         ADDRESS OF X MARKET LIST               
ACLTLST  DS    A                         ADDRESS OF CLIENT LIST                 
ACLTLSTX DS    A                         ADDRESS OF END OF CLIENT LIST          
*                                                                               
SV37ADR  DS    A                                                                
SV37ADR2 DS    A                                                                
*                                                                               
SVTSRREC DS    XL(HRECLN)                                                       
LASTFLDS DS    0XL4                                                             
LASTCLT  DS    XL2                                                              
LASTPRD  DS    XL1                                                              
LASTPRD2 DS    XL1                                                              
LASTEST  DS    XL1                                                              
SVINFLAG DS    XL1                       SAVE AINFFLAG                          
SVSPRVSR DS    CL2                       SAVE ORIGINAL SUPERVISOR               
SVUSERCD DS    CL4                       SAVE ORIGINAL USER CODE                
SVRVSNDT DS    XL3                       SAVE ORIGINAL REVISION DATE            
SVDDSPV  DS    CL6                       SAVE SUPV WHO SET DUE DATE             
SVDDDATE DS    XL3                       SAVE DATE DUE DATE SET                 
REVFLAG  DS    XL1                                                              
REVFTBD  EQU   X'80'               OVERRIDE DUE DATE TO TBD                     
REVFSUPV EQU   X'40'               OVERRIDE SUPV AND DATE                       
*                                                                               
ERRORCD  DS    X                         0=SUCCESS, 1=FAIL, 2=DUP KEY           
*                                                                               
BLANK    EQU   C'0'                                                             
PENDING  EQU   1                                                                
*                                                                               
OVFLAG1  DS    X                                                                
NEWREV   EQU   X'80'                     CREATE A NEW REVISION                  
GOALMKT  EQU   X'40'                     MARKET FOUND THROUGH GOALS             
DUEDTCH  EQU   X'20'                     DUE DATE CHANGED                       
REVACMKT EQU   X'10'                     REVISING ALL ACTIVE MARKTS             
NOUPDATE EQU   X'08'                     TESTING WITH U=N                       
FLTDTCH  EQU   X'04'                     FLIGHT DATES CHANGED                   
STADTCH  EQU   X'02'                     START DATE CHANGED                     
MKTDUECH EQU   X'01'                     MARKET DUE DATE CHANGED                
*                                                                               
DWNFLAG  DS    X                                                                
MYCLIENT EQU   X'80'                     CLIENT IS "MY CLIENT"                  
MKTNTVLD EQU   X'40'                     MARKET NOT VALID                       
SENT02   EQU   X'20'                     SENT 02 ALREADY                        
CLTLOCK  EQU   X'10'                     CAN'T VIEW THIS CLIENT                 
CHKLOCK  EQU   X'08'                     CHECK CLT LOCK                         
*                                                                               
OVMFLAG1 DS    X                         OVERLAY MARKET FLAG                    
SUPAPVD  EQU   X'80'                                                            
ADDCNLD  EQU   X'40'                     RE-ADD A CANCELLED MARKET              
MKTCANLD EQU   X'20'                     THIS MARKET IS CANCELLED               
BYRPNDG  EQU   X'10'                     BUYER STATUS IS PENDING                
MKTDELD  EQU   X'08'                     THIS MARKET IS DELETED                 
ADDDELD  EQU   X'04'                     RE-ADD A DELETED MARKET                
NOREADD  EQU   X'02'               DO NOT READD MKT/UPDATE DUE DATE             
*                                                                               
OVMFLAG2 DS    X                         OVERLAY MARKET FLAG                    
ORDERED  EQU   X'80'                                                            
CNFRMD   EQU   X'40'                                                            
NOTCNFMD EQU   X'20'                     A STATION HAS NOT BEEN CONFRMD         
STAEXST  EQU   X'10'                     A STATION RECORD EXISTS                
SNDDTLS  EQU   X'08'                     SEND ALL STATION DETAILS               
NOTORDRD EQU   X'04'                     A STATION HAS NOT BEEN ORDERED         
*                                                                               
MKTNTRV  EQU   0                         NEVER REVISED                          
MKTREV   EQU   1                         REVISED                                
MKTRVLST EQU   2                         REVISED IS LATEST STATUS               
MKTEDLST EQU   3                         EDIT IS LATEST STATUS                  
*                                                                               
* STATION ORDERED STATUS                                                        
STAORSTA DS    X                                                                
STABLNK  EQU   0                         NO STATIONS ORDERED YET                
STAPRTL  EQU   1                         SOME STATIONS ORDERED                  
STABLSTA EQU   2                         BLANK, BUT HAS STATIONS                
STAORDRD EQU   3                         ALL STATIONS ORDERED                   
STACNFRM EQU   10                        ALL STATIONS CONFIRMED                 
STACNFDT DS    XL3                       STATION CONFIRMED DATE                 
OLDSTADT DS    XL3                       OLD START DATE                         
OLDDUEDT DS    XL3                       OLD START DATE                         
BYTESNT  DS    F                         BYTES SENT FALINK                      
SEND0B   DS    C                         SEND 0B HEADER (Y=YES)                 
PROCTBD  DS    C                         Y WHEN READING FOR TBDS                
TEMPDTS  DS    XL6                                                              
SVSTAT   DS    0XL3                      SAVED STATUS                           
SVAPP    DS    XL1                                                              
SVREJ    DS    XL1                                                              
SVCOMP   DS    XL1                                                              
NXTSEQ   DS    XL1                 NEXT SEQ NUMBER FOR 37 ELEMS                 
HISTXIST DS    CL1                 ARE THERE 37 ELEMS YET                       
LASTVAR  DS    XL3                 VARIOUS DATE APR5 AND APR6 1980              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENOFC                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSNV                                                       
GOLRECD  DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPSDE02   10/10/11'                                      
         END                                                                    
