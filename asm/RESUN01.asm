*          DATA SET RESUN01    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T81401A,*                                                                
         TITLE 'T81401 - RESUN01 - SONNET'                                      
*                                                                               
*******************************************************************             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
*  02JUN99 RHV - ATERNATE '9F' KEY FOR NBC HOME MKT ORDERS        *             
*                                                                 *             
*  20JUN00 RHV - HOME MARKET SETUP REVISED                        *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
T81401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1401**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         OI    GENSTAT4,NODELLST   NO DELETE FROM LIST                          
*                                                                               
         CLI   MODE,RECDEL                                                      
         BE    INVAERR                                                          
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,XRECPUT        RECORD HAS JUST BEEN PUT                     
         BE    XPUT                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************                            
*              VALIDATE  KEY                                                    
****************************************************                            
VKEY     XC    SVKEY,SVKEY                                                      
         XC    SVBOXID,SVBOXID                                                  
         XC    SVSTAT,SVSTAT                                                    
*                                                                               
         MVI   KEYTYPE,X'8F'       DEFAULT TO USE 8F KEY                        
*                                                                               
         XC    WORK,WORK           GET CONTRACT PROG PROFILES                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),AGENCY                                                  
         GOTO1 (RFGPROF,VREPFACS),DMCB,('RREPQCNT',WORK),0,0,DUB                
         TM    WORK+2+CNTRPEAB,CNTRPEAA   CONTRACT PROF #46                     
         BZ    VK10                NO - SKIP ADDITIONAL CHECK                   
*                                                                               
         GOTOX (RFGETID,VREPFACS),DMCB,(RA),WORK,0,DUB                          
         CLI   WORK+4,C'L'         5 POSITION OF SIGNON = 'L'?                  
         BNE   *+8                                                              
         MVI   KEYTYPE,X'9F'       USE NBC HOME MKT '9F' KEY                    
*                                                                               
VK10     DS    0H                                                               
K        USING RCON8FTP,SVKEY                                                   
         MVC   K.RCON8FTP,KEYTYPE     PASSIVE CONTRACT KEY                      
         MVC   K.RCON8FRP,AGENCY                                                
                                                                                
         LA    R2,SOMSTATH         STATION                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
VK20     GOTO1 VALISTA,DMCB        RETURNS CALL LETTERS IN WORK                 
*                                                                               
         CLI   KEYTYPE,X'9F'                                                    
         BE    *+12                                                             
         CLI   TWAACCS,C'$'        CHECK STATION ACCESS                         
         BNE   INVAERR                                                          
*                                                                               
         L     R6,AIO              STATION REC SHOULD SIT IN AIO                
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK25     BAS   RE,NEXTEL                                                        
         BNE   INVERR                                                           
         USING RSTASOEL,R6                                                      
         CLC   RSTASID,TWAORIG     ACCESS ONLY TO OWN STATION                   
         BNE   VK25                                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              STATION REC SHOULD SIT IN AIO                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVERR                                                           
         USING RSTAELEM,R6                                                      
         MVC   SVTRAFIC,RSTATRAF                                                
         DROP  R6                                                               
*                                                                               
         MVC   K.RCON8FSA,WORK                                                  
         MVC   SVSTAT,WORK                                                      
*                                                                               
VK30     LA    R2,SOMBOXIH         BOXID                                        
         CLI   5(R2),0                                                          
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST      IF LIST ACTION                               
         BE    VK40                OK NO BOXID                                  
         B     MISSERR                                                          
VK35     DS    0H                                                               
         CLC   =C'NEW',8(R2)        ,,SPECIAL 'NEW'                             
         BNE   VK37                                                             
         CLI   ACTNUM,ACTLIST       ,,FOR LIST ACTION ONLY                      
         BNE   INVERR                                                           
         MVC   SVBOXID(3),8(R2)                                                 
         B     VK40                                                             
VK37     GOTO1 CHKBOXID,DMCB,SVSTAT,SOMBOXI                                     
         BNE   INVERR                                                           
         MVC   K.RCON8FIN,SOMBOXI                                               
         MVC   SVBOXID,SOMBOXI                                                  
*                                                                               
VK40     CLI   ACTNUM,ACTLIST      IF LIST ACTION                               
         BE    VK50                SKIP CONTRACT NUMBER                         
*                                                                               
         LA    R2,SOMCONTH         CONTRACT NUMBER                              
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMPLEMENT          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONPTYP,R6                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK       CONTRACT NUMBER                              
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INVERR                                                           
*                                                                               
         UNPK  WORK(8),DUB+3(5)    PACK WITHOUT SIGN                            
         PACK  DUB(5),WORK(9)                                                   
         MVC   K.RCON8FCN,DUB                                                   
* READ FOR KEY HERE SINCE NOT ENTIRE KEY IS BUILT                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
VK48     CLC   KEY(8),KEYSAVE                                                   
         BNE   VKXX                   NOT FOUND                                 
         CLC   KEY+13(4),KEYSAVE+13   CONTRACT NUMBER                           
         BE    VK49                                                             
         GOTO1 SEQ                                                              
         B     VK48                                                             
                                                                                
VK49     MVC   SVKEY,KEY          FOUND/PASS TO GENCON TO READ AGAIN            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VKXX                                                             
*                                                                               
VK50     CLI   ACTNUM,ACTLIST      FILTER FOR LIST ACTION ONLY                  
         BNE   VKXX                                                             
         LA    R2,SOLFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VKXX                                                             
*                                                                               
VKXX     MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  K                                                                
         EJECT                                                                  
****************************************************                            
* DISPLAY KEY                                                                   
****************************************************                            
DKEY     L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         LA    R2,SOMSTATH         STATION                                      
         MVC   8(5,R2),RCONKSTA                                                 
         OI    6(R2),X'80'                                                      
                                                                                
*                                                                               
         LA    R2,SOMBOXIH         BOX ID                                       
         MVI   ELCODE,X'A3'                                                     
         BAS   RE,GETEL                                                         
         BNE   DK30                                                             
*                                                                               
*-------------------*                                                           
* GET SENT TO BOXID                                                             
*-------------------*                                                           
         ZIC   R1,1(R6)                                                         
         CHI   R1,RCONSOVQ         NEW ELEMENT?                                 
         BE    DK30                YES                                          
*                                                                               
         AHI   R1,-RCONSOVQ        SUBTRACT OVERHEAD                            
         SR    R0,R0                                                            
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEMENTS               
*                                                                               
         LA    R6,RCONSONM-RCONSON(R6)                                          
DK10     OC    0(3,R6),0(R6)       ID ?                                         
         BZ    DK20                                                             
         OC    3(2,R6),3(R6)       DATE SENT?                                   
         BZ    DK20                NO DATE /THIS IS 'TO BOX'                    
         LA    R6,6(R6)            BUMP TO NEXT ELEM                            
         BCT   R1,DK10                                                          
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
DK20     MVC   8(3,R2),0(R6)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
*-------------------*                                                           
* GET CONTRACT #                                                                
*-------------------*                                                           
DK30     DS    0H                                                               
         LA    R2,SOMCONTH         CONTRACT NUMBER                              
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         UNPK  WORK(9),RCONKCON(5)    PACKED WITHOUT SIGN                       
         MVC   8(8,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************                            
*              VALIDATE RECORD                                                  
****************************************************                            
VREC     EQU   *                                                                
         LA    R2,SOMFRBXH         FROM BOX ID                                  
         TM    1(R2),X'20'         IS IT PROTECTED?                             
         BO    VR10                                                             
*                                                                               
         CLI   5(R2),0             NO - SO USER MUST FILL IT IN                 
         BE    MISSERR                                                          
         MVC   SVKEY2,KEY          SAVE KEY                                     
*                                                                               
         GOTO1 CHKBOXID,DMCB,SOMSTAT,SOMFRBX                                    
         BNE   INVERR                                                           
                                                                                
VR10     LA    R2,SOMTOBXH         TO BOX ID                                    
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         GOTO1 CHKBOXID,DMCB,SOMSTAT,SOMTOBX                                    
         BNE   INVERR                                                           
*                                                                               
         LA    R2,SOMAPPRH         APPROVED?                                    
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   INVERR                                                           
                                                                                
         MVC   KEY,SVKEY2          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              RESET DMWORK                                 
*                                                                               
* - UPDATE A3 ELEMENT                                                           
*                                                                               
         L     R6,AIO              DELETE COMMENTS                              
         LA    R6,RCONELEM-RCONREC(R6)                                          
         MVI   ELCODE,X'A5'                                                     
VR012    DS    0H                                                               
         BAS   RE,FIRSTEL                                                       
         BNE   VR014                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIO),(R6),0                                    
         B     VR012                                                            
*                                                                               
VR014    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A3'        SAVE & DELETE ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                  DELETE ELEMENT                               
         GOTO1 VRECUP,DMCB,(C'R',AIO),(R6),0                                    
*                                                                               
         CLI   ELEM+1,RCONSOVQ     'NEW' ELEM ?                                 
         BNE   VR30                NO                                           
*                                                                               
* 'NEW' CONTRACT - NO MINI BOXID ELEMS                                          
*                                                                               
         LA    R3,ELEM                                                          
         USING RCONSON,R3                                                       
         MVC   RCONSONM(3),SOMFRBX                      FROM                    
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,RCONSONM+3)    DATE                    
         MVC   RCONSONM+5(1),SOMAPPR                    APPROVAL                
         MVC   RCONSONM+6(3),SOMTOBX                    TO                      
         MVI   RCONSONL,RCONSOVQ+(2*L'RCONSONM)         NEW EL LENGTH           
         B     VR40                                                             
         DROP  R3                                                               
*                                                                               
VR30     DS    0H                                                               
         ZIC   R1,ELEM+1           GET ELEM LENGTH                              
         AHI   R1,-RCONSOVQ        SUBTRACT OVERHEAD                            
         SR    R0,R0                                                            
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEMENTS               
*                                                                               
         CHI   R1,6                6 MINI ELEMENTS?                             
         BL    VR32                NO                                           
*                                                                               
         ZIC   R1,ELEM+1           YES - REMOVE THE 1ST ONE                     
         AHI   R1,-(L'RCONSONM)                                                 
         STC   R1,ELEM+1                                                        
*                                                                               
*                                  NOW MOVE 5 MINI ELEMENTS                     
         LA    R3,ELEM+(RCONSONM-RCONSONE)                                      
         MVC   0((L'RCONSONM*5),R3),L'RCONSONM(R3)                              
*                                                                               
*                                  CLEAR THE END                                
         XC    (L'RCONSONM*5)(L'RCONSONM,R3),(L'RCONSONM*5)(R3)                 
*                                                                               
VR32     DS    0H                                                               
         ZIC   R1,ELEM+1           GET ELEM LENGTH                              
         AHI   R1,-(L'RCONSONM)    SUBTRACT 1 MINI ELEMENT                      
*                                                                               
         LA    R3,ELEM(R1)         POINT R3 TO LAST MINI ELEMENT                
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,3(R3))   TODAY'S DATE                  
         MVC   5(1,R3),SOMAPPR                    APPROVAL                      
*                                                                               
         LA    R3,L'RCONSONM(R3)   POINT TO THE NEW ELEMENT                     
         MVC   0(3,R3),SOMTOBX                    TO BOX ID                     
*                                                                               
         ZIC   R1,ELEM+1           GET LENGTH INTO R1                           
         LA    R1,L'RCONSONM(R1)   INCREASE BY LENGTH OF NEW MINI ELEM          
         STC   R1,ELEM+1                                                        
                                                                                
VR40     DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R3,SOMCOM1H                                                      
VR042    DS    0H                                                               
         SR    R1,R1               GET INPUT LENGTH                             
         ICM   R1,1,5(R3)                                                       
         BZ    VR046               NO INPUT - SKIP COMMENT                      
*                                                                               
         XC    ELEM,ELEM                                                        
E        USING RCONSCEL,ELEM                                                    
         MVI   E.RCONSCCO,X'A5'                                                 
         LA    R0,RCONSCOV(R1)     ELEMENT LENGTH                               
         STC   R0,E.RCONSCLN                                                    
         BCTR  R1,0                EX LENGTH OF COMMENT                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   E.RCONSCCM(0),8(R3)                                              
         DROP  E                                                                
*                                                                               
         L     R6,AIO1             FIND A HOME                                  
         LA    R6,RCONELEM-RCONREC(R6)                                          
VR044    DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BE    *+12                YES                                          
         CLI   0(R6),X'A5'                                                      
         BNH   VR044                                                            
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',AIO1),ELEM,(R6)                                
*                                                                               
VR046    DS    0H                                                               
         LA    R0,SOMCOM2H                                                      
         CR    R3,R0                                                            
         BE    VR050               SECOND COMMENT DONE, EXIT                    
*                                                                               
         LA    R3,SOMCOM2H         DO SECOND COMMENT                            
         B     VR042                                                            
*                                                                               
VR050    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************                            
* RECORD HAS JUST BEEN 'PUT' - ADD/CHANGE PASSIVE KEY                           
****************************************************                            
XPUT     DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,KEY+28         SAVE DISK ADDRESS                            
*                                                                               
         MVC   WORK(3),SOMFRBX                                                  
                                                                                
XPUT01   XC    KEY,KEY                                                          
         OC    WORK(3),SPACES                                                   
K        USING RCON8FTP,KEY                                                     
         MVC   K.RCON8FTP,KEYTYPE                                               
         MVC   K.RCON8FRP,AGENCY                                                
         MVC   K.RCON8FSA,RCONKSTA                                              
         OC    K.RCON8FSA,SPACES                                                
         GOTO1 HIGH                                                             
                                                                                
XPUT02   CLC   KEY(RCON8FDT-RCON8FTP),KEYSAVE                                   
         BNE   XPUT03                                                           
         CLC   K.RCON8FIN,WORK          BOXID                                   
         BNE   *+14                                                             
         CLC   K.RCON8FCN,RCONKCON      CONTRACT #                              
         BE    XPUT05                                                           
*                                                                               
         GOTO1 SEQ                                                              
         B     XPUT02                                                           
*                                                                               
XPUT03   DS    0H                  MUST BE 'NEW' KEY                            
         CLC   =C'NEW',WORK                                                     
         BNE   *+6                                                              
         DC    H'0'                MUST EXIST                                   
         MVC   WORK(3),=C'NEW'                                                  
         B     XPUT01                                                           
*                                                                               
* KEY FOUND                                                                     
*                                                                               
XPUT05   OI    KEY+27,X'80'         DELETE OLD KEY                              
         GOTO1 WRITE                                                            
*                                                                               
* BUILD NEW 'TO' KEY                                                            
*                                                                               
         NI    KEY+27,X'FF'-X'80'   TURN OFF DELETE                             
         MVC   K.RCON8FIN,SOMTOBX     SET NEW 'TO' ID                           
         DROP  K                                                                
                                                                                
         OI    DMINBTS,X'08'                      PASS DELETED RECS             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                TURN IT OFF                   
         CLC   KEY(27),KEYSAVE     IS IT ON FILE ?                              
         BNE   XPUT15              NO                                           
         NI    KEY+27,X'FF'-X'80'  YES/TURN OFF DELETE                          
         GOTO1 WRITE                                                            
         B     XPUTX                                                            
                                                                                
XPUT15   MVC   KEY,KEYSAVE         RESET NEW KEY                                
         MVC   KEY+28(4),FULL      INSERT DISK ADDRESS                          
         GOTO1 ADD                                                              
                                                                                
XPUTX    B     XIT                                                              
         EJECT                                                                  
****************************************************                            
* DISPLAY RECORD                                                                
****************************************************                            
DREC     EQU   *                                                                
         LA    R2,SOMFRBXH                                                      
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         BAS   RE,DOSONNET         FILLS IN SONBLK WITH SONNET DATA             
*                                                                               
*---------------*                                                               
* FROM ID FIELD *                                                               
*---------------*                                                               
         CLI   SONTOBOX,X'40'      MEMID?                                       
         BNH   DR10                NO - VIRGIN CONTRACT                         
*                                                                               
         MVC   SOMFRBX,SONTOBOX    MOVE IN MEM ID                               
         OI    1(R2),X'20'         PROTECT IT                                   
         OI    6(R2),X'80'         FOUT IT                                      
         B     DR12                                                             
*                                                                               
* ONLY GETS HERE IF 'FROM BOX' IS BLANK - VIRGIN CONTRACT FOR MAILBOX           
* USER MUST FILL IT IN                                                          
*                                                                               
DR10     DS    0H                                                               
         OI    1(R2),X'08'         HIGH INTENSITY                               
         OI    6(R2),X'80'         FOUT IT ?                                    
*                                                                               
DR12     DS    0H                                                               
*                                                                               
*-------------*                                                                 
* TO ID FIELD *                                                                 
*-------------*                                                                 
*        LA    R2,SOMTOBXH          TO BOX - TO BE FILLED BY USER               
*                                   UNLESS WE GIVE USER ABILITY TO              
*                                   CHANGE THEIR INPUT                          
*                                                                               
*----------------*                                                              
* APPROVED FIELD *                                                              
*----------------*                                                              
*        LA    R2,SOMAPPRH         APPROVED?                                    
*                                  TO BE FILLED IN                              
*                                                                               
*----------------*                                                              
* SENT DATE/TIME *                                                              
*----------------*                                                              
         LA    R2,SOMSFRPH         SENT FROM REP DATE/TIME                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(20),WORK                                                    
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BNO   DR32                                                             
*                                                                               
         MVC   WORK(2),RCONSRDT                  SAVE REP SENT DATE             
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK+2,6                                     
*                                                                               
         DROP  R6                                                               
DR32     L     R6,AIO              MAKEGOOD OFFER ?                             
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    DR34                YES                                          
         OC    WORK(5),WORK        NO-ORDER MUST BE LAST SENT BY REP            
         BNZ   DR40                                                             
         DC    H'0'                SHOULD NEVER GET HERE                        
         USING RCONMGEL,R6                                                      
DR34     TM    RCONMGFG,X'40'      YES - LAST SENT BY REP?                      
         BZ    DR40                                                             
         MVC   WORK+10(2),RCONMGDT     ,,DO OLD ELEMENT DATA                    
         MVC   WORK+12(3),RCONMGTM                                              
         CLI   RCONMGLN,RCONMGLQ       ,,UNLESS WE HAVE A NEW ELEMENT           
         BE    DR36                    NO                                       
         MVC   WORK+10(2),RCONRMDT     YES,NEW ELEM                             
         MVC   WORK+12(3),RCONRMTM                                              
DR36     CLC   WORK(2),WORK+10     COMPARE ORDER DATE TO MKGD SENT DATE         
         BH    DR40                                                             
         BL    DR38                                                             
* IF SAME DATE/ COMPARE TIME SENT                                               
         CLC   WORK+2(3),WORK+12   COMPARE SENT TIMES                           
         BH    DR40                                                             
DR38     MVC   WORK(2),WORK+10     MKGD ELEM GETS PREFERENCE                    
         MVC   WORK+2(3),WORK+12                                                
         DROP  R6                                                               
*                                                                               
DR40     GOTO1 DATCON,DMCB,(2,WORK),(7,8(R2))          DATE                     
         GOTO1 HEXOUT,DMCB,WORK+2,WORK+10,3,0                                   
         MVC   14(2,R2),WORK+10                                                 
         MVI   16(R2),C':'                                                      
         MVC   17(2,R2),WORK+12                                                 
                                                                                
         OI    6(R2),X'80'         FOUT IT                                      
*                                                                               
*--------------*                                                                
* SENT HISTORY *                                                                
*--------------*                                                                
         LA    R2,SOMHISTH         SENT HISTORY                                 
         LA    R0,SOMHENDH         END OF HISTORY LIST                          
         LA    R3,SONBLK                                                        
DR44     OC    0(3,R3),0(R3)       MEMBER ID?                                   
         BZ    DR46                                                             
         OC    3(2,R3),3(R3)       DATE?                                        
         BZ    DR46                                                             
         MVC   8(3,R2),0(R3)                                                    
         GOTO1 DATCON,DMCB,(2,3(R3)),(7,12(R2))                                 
         CLI   5(R3),C'Y'          APPROVED?                                    
         BNE   *+10                                                             
         MVC   18(8,R2),=C'APPROVED'                                            
         OI    6(R2),X'80'         FOUT IT                                      
                                                                                
         LA    R3,6(R3)            BUMP SONBLK                                  
         ZIC   RE,0(R2)            BUMP SCREEN OUTPUT LINE                      
         AR    R2,RE                                                            
         CR    R2,R0               CHECK END OF HISTORY LIST                    
         BNH   DR44                                                             
*                                                                               
DR46     EQU   *                                                                
*                                                                               
*--------------*                                                                
* LAST COMMENT *                                                                
*--------------*                                                                
         MVC   SOMLCM1,SONCOM1                                                  
         OI    SOMLCM1H+6,X'80'                                                 
         MVC   SOMLCM2,SONCOM2                                                  
         OI    SOMLCM2H+6,X'80'                                                 
*                                                                               
*--------*                                                                      
* AGENCY *                                                                      
*--------*                                                                      
         L     R6,AIO              AGENCY                                       
         USING RCONREC,R6                                                       
                                                                                
         MVC   SVKEY2,KEY          SAVE KEY                                     
                                                                                
         LA    R2,SOMAGYNH                                                      
         MVC   WORK(4),RCONKAGY                                                 
         BAS   RE,GETAGYN          RETURNS AGY NAME IN WORK                     
         MVC   8(4,R2),RCONKAGY                                                 
         MVI   12(R2),C'-'                                                      
         MVC   12(2,R2),RCONKAOF                                                
         MVC   15(20,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
*------------*                                                                  
* ADVERTISER *                                                                  
*------------*                                                                  
         LA    R2,SOMADVNH         ADVERTISER                                   
         MVC   WORK(4),RCONKADV                                                 
         BAS   RE,GETADVN          RETURNS ADVERTISER NAME IN WORK              
         MVC   8(4,R2),RCONKADV                                                 
         MVC   15(20,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
*---------*                                                                     
* PRODUCT *                                                                     
*---------*                                                                     
         LA    R2,SOMPRODH         PRODUCT                                      
         MVC   WORK(3),(RCONPRD)                                                
         BAS   RE,GETPRDN                                                       
         MVC   8(3,R2),RCONPRD                                                  
         MVC   15(20,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
*--------*                                                                      
* OFFICE *                                                                      
*--------*                                                                      
         LA    R2,SOMOFFNH         OFFICE                                       
         MVC   WORK(2),RCONKOFF                                                 
         BAS   RE,GETOFFN                                                       
         MVC   8(2,R2),RCONKOFF                                                 
         MVC   15(20,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
*-------------*                                                                 
* SALESPERSON *                                                                 
*-------------*                                                                 
         LA    R2,SOMSALSH         SALESPERSON                                  
         MVC   WORK(3),RCONSAL                                                  
         BAS   RE,GETSLSN                                                       
         MVC   8(3,R2),RCONSAL                                                  
         MVC   15(20,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
*---------------------------------------------------------(                     
         MVC   KEY,SVKEY2          RESET KEY                                    
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     EQU   *                                                                
         OC    PFAID,PFAID         IF PFKEY PRESSED                             
         BZ    LR00                                                             
         MVC   KEY(27),LISTKSV     ENSURE WE KEEP CURRENT LIST SCREEN           
         B     LR10                                                             
                                                                                
LR00     OC    KEY(27),KEY                                                      
         BZ    LR04                                                             
* SINCE WE'RE USING PASSIVE KEY TO GENERATE LIST SCREEN                         
* NEED TO SET UP PASSIVE KEY FROM CONTRACT REC TO CONTINUE LIST                 
* AFTER SELECT                                                                  
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLC   RCONKTYP,=X'0C00'   CONTRACT RECORD?                             
         BNE   LRX                 NO - THAT'S ALL FOLKS                        
K        USING RCON8FTP,KEY                                                     
         MVC   K.RCON8FTP,KEYTYPE                                               
         MVC   K.RCON8FRP,RCONKREP                                              
         MVC   K.RCON8FSA,RCONKSTA                                              
         MVC   K.RCON8FCN,RCONKCON                                              
*                                                                               
         MVI   ELCODE,X'A3'                                                     
         USING RCONSON,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   LRSEQ               NOT THERE, SKIP TO NEXT RECORD               
*                                                                               
         MVC   K.RCON8FDT,RCONSDTE                                              
         DROP  R6                                                               
*                                                                               
         CLI   1(R6),RCONSOVQ      'NEW'ELEM ?                                  
         BNE   *+14                                                             
         MVC   K.RCON8FIN,=C'NEW'    YES                                        
         B     LR10                                                             
*                                                                               
         ZIC   R1,1(R6)                                                         
         AHI   R1,RCONSOVQ         SUBTRACT OVERHEAD                            
         SR    R0,R0                                                            
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF EACH MINI ELEM              
         LTR   R1,R1               R1=NUMBER OF MINI ELEMENTS                   
         BP    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
         LA    R6,RCONSOVQ(R6)     POINT TO START OF MINI ELEMS                 
LR02     OC    3(2,R6),3(R6)       DATE?                                        
         BNZ   *+14                                                             
         MVC   K.RCON8FIN,0(R6)    NO DATE - THIS IS 'TO' ID                    
         B     LR10                                                             
         DROP  K                                                                
*                                                                               
         LA    R6,6(R6)                                                         
         BCT   R1,LR02                                                          
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
LR04     XC    KEY,KEY             FIRST TIME SET UP KEY                        
K        USING RCON8FTP,KEY                                                     
         MVC   K.RCON8FTP,KEYTYPE                                               
         MVC   K.RCON8FRP,AGENCY                                                
         MVC   K.RCON8FSA,SVSTAT                                                
         MVC   K.RCON8FIN,SVBOXID                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
LR10     DS    0H                                                               
         MVC   LISTKSV,KEY         SAVE KEY FOR PFKEY JUMPS                     
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(RCON8FSA-RCON8FTP),KEYSAVE                                   
         BNE   LRX                                                              
*                                                                               
         CLI   SVBOXID,X'40'       BOXID                                        
         BNH   LR22                                                             
         CLC   SVBOXID,K.RCON8FIN                                               
         BNE   LRSEQ                                                            
*                                                                               
LR22     CLI   SVSTAT,X'40'        STATION                                      
         BNH   *+14                                                             
         CLC   SVSTAT,K.RCON8FSA                                                
         BNE   LRSEQ                                                            
*                                                                               
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
         MVC   LISTAR,SPACES                                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
**********************************************                                  
         MVC   LSAGY(4),RCONKAGY   AGENCY/OFFICE                                
         MVI   LSAGY+4,C'-'                                                     
         MVC   LSAGY+5(2),RCONKOFF                                              
*                                                                               
************************************************                                
*                                   ADVERTISER                                  
         MVC   SVKEY2,KEY             SAVE KEY                                  
         MVC   WORK(4),RCONKADV                                                 
         BAS   RE,GETADVN                                                       
         MVC   LSADVR,WORK         ADVERTISER NAME                              
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                RESET READ SEQ                               
*                                                                               
*********************************************************                       
*                                  PRODUCT NAME                                 
         MVC   SVKEY2,KEY                                                       
         BAS   RE,GETPRDN                                                       
         MVC   LSPROD,WORK                                                      
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                RESET READ SEQ                               
*                                                                               
*****************************************************************               
*                                      BOXID                                    
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVC   LSBOXID,=C'NEW'                                                  
         MVI   ELCODE,X'A3'        GET SONNET ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   LRSEQ               NOT THERE, SKIP TO NEXT RECORD               
         ZIC   R1,1(R6)            GET NUMB OF MINI ELEMS INTO R1               
         CHI   R1,RCONSOVQ         IF NEW                                       
         BE    LR30                THAT'S ALL HERE                              
*                                                                               
         SH    R1,RCONSOVQ         SUBTRACT OVERHEAD                            
         SR    R0,R0                                                            
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEMS                  
         LA    R3,RCONSONM-RCONSON(R6)                                          
LR24     OC    0(3,R3),0(R3)       BOX ID?                                      
         BZ    LR30                BLANK                                        
         OC    3(2,R3),3(R3)       DATE SENT                                    
         BZ    LR26                NO DATE - THIS IS 'TO' BOX                   
*                                                                               
         LA    R3,6(R3)            BUMP MINI ELEM                               
         BCT   R1,LR24                                                          
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
LR26     MVC   LSBOXID,0(R3)       SET BOXID                                    
*                                                                               
**********************************************************                      
LR30     DS    0H                   FLIGHT DATES                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,RCONDATE),(4,LSDATES)                             
         MVI   LSDATES+5,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(4,LSDATES+6)                         
                                                                                
**************************************************************                  
LR35     L     R6,AIO                       CONTRACT NUMBER                     
         USING RCONREC,R6                                                       
         XC    LSCON,LSCON                                                      
         MVC   WORK(20),SPACES                                                  
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RCONKCON                                              
         OI    WORK+24,X'0F'                                                    
         UNPK  WORK(9),WORK+20(5)                                               
         LA    RE,WORK                                                          
         LA    RF,7                                                             
         CLI   0(RE),C'0'          REMOVE LEADING ZERO'S                        
         BH    *+14                                                             
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         B     *-14                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSCON,0(RE)                                                      
                                                                                
*****************************************************************               
LR60     DS    0H                            BUDGET DOLLARS                     
         L     R6,AIO                                                           
         XC    FULL,FULL                                                        
         SR    R1,R1                                                            
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR62     BAS   RE,NEXTEL                                                        
         BNE   LR64                                                             
         USING RCONBKEL,R6                                                      
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     LR62                                                             
LR64     SR    R0,R0                                                            
         D     R0,=F'100'          DROP PENNIES                                 
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(13,LSBUDGT),FLOAT=$,ALIGN=LEFT,ZERO=NOBLANK           
                                                                                
**************************************************************                  
         L     R6,AIO                         VERSION NUMBER                    
         USING RCONREC,R6                                                       
         CLI   RCONMOD,X'FF'       MODIFICATION?                                
         BNE   *+12                YES                                          
         MVI   LSVERN,C'N'                                                      
         B     LR66                                                             
*                                                                               
         EDIT  (B1,RCONMOD),(1,LSVERN+3),ZERO=NOBLANK                           
LR66     MVI   LSVERN+2,C'-'                                                    
         MVI   LSVERN+4,C'-'                                                    
         USING RCONSEND,R6                                                      
                                                                                
* CHECK SEND ELEMENT                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   RCONSRV,RCONSSV     REP OR STATION VERSION HIGHER?               
         BH    DATAN40                                                          
* STATION                                                                       
         EDIT  (1,RCONSSV),(2,LSVERN+5)                                         
         TM    RCONSENF,X'10'      STATION WORK IN PROGRESS?                    
         BO    LR70                                                             
         MVC   LSVERN(2),=C'WP'   YES, DISPLAY SO                               
         B     LR70                                                             
                                                                                
* REP                                                                           
DATAN40  DS    0H                                                               
         EDIT  (1,RCONSRV),(2,LSVERN+5)                                         
         TM    RCONSENF,X'20'      REP WORK IN PROGRESS?                        
         BO    LR70                                                             
         MVC   LSVERN(2),=C'WP'     YES, DISPLAY SO                             
         B     LR70                                                             
*                                                                               
                                                                                
                                                                                
********************************************************                        
LR70     DS    0H                                    AGE                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'A3'        GET SONNET ELEMENT                           
         USING RCONSON,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   LRSEQ               NOT THERE, SKIP TO NEXT RECORD               
         GOTO1 DATCON,DMCB,(2,RCONSDTE),(0,WORK)                                
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK+6)                                
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         EDIT  (B2,DMCB+8),LSAGE                                                
*                                                                               
LR80     DS    0H                                                               
         GOTO1 GETREC              MAKE SURE DMWORK HAS CONTRACT REC            
         GOTO1 LISTMON                                                          
         DROP  R6                                                               
                                                                                
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
****************************************************                            
* CHECKBOXID                                                                    
*       P1 - A(STATION)                                                         
*       P2 - A(BOXID)                                                           
****************************************************                            
CHKBOXID NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
*                                                                               
         XC    KEY,KEY                                                          
K        USING RBOXKEY,KEY                                                      
         MVI   K.RBOXKTYP,RBOXKTYQ                                              
         MVC   K.RBOXKREP,AGENCY                                                
         MVC   K.RBOXKSTA,0(R2)    SET IT IN KEY                                
         OC    K.RBOXKSTA,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RBOXKEY),KEYSAVE      CHECK ID/REP                         
         BNE   CBOXNO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              SEARCH FOR BOXID ELEMENT                     
         MVI   ELCODE,RBOXIDEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   CBOXNO                                                           
*                                                                               
         USING RBOXIDEL,R6                                                      
         OC    0(L'RBOXIDCD,R3),SPACES                                          
CBOX010  DS    0H                                                               
         CLC   RBOXIDCD,0(R3)                                                   
         BE    CBOXYES                                                          
         DROP  R6                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    CBOX010                                                          
*                                                                               
CBOXNO   LTR   RB,RB                                                            
         B     *+6                                                              
CBOXYES  DS    0H                                                               
         CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
********************************************************                        
* GET SONNET ELEMENT AND FILLS OUT SONNET BLOCK                                 
********************************************************                        
DOSONNET NTR1                                                                   
         LA    R0,SONBLK           SONNET BLOCK                                 
         LA    R1,L'SONBLK                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SONTOBOX,SONTOBOX                                                
         XC    SONCOM1,SONCOM1                                                  
         XC    SONCOM2,SONCOM2                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A3'        GET SONNET ELEM                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONSON,R6                                                       
         ZIC   R1,RCONSONL                                                      
         CHI   R1,RCONSOVQ         LENGTH = OVERHEAD?                           
         BE    DSX                 YES - NEW CONTRACT, EXIT NOW                 
*                                                                               
         AHI   R1,-RCONSOVQ        SUBTRACT OVERHEAD TO GET LENGTH OF           
         SR    R0,R0                 MINI ELEMENT LIST                          
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEM                   
*                                                                               
         LA    R2,SONBLK                                                        
         LA    R3,RCONSONM         POINT TO MINI ELEMENT LIST                   
DS10     OC    0(3,R3),0(R3)       ANY MEMBER BOXID ?                           
         BZ    DS20                NO                                           
*                                                                               
         MVC   0(L'RCONSONM,R2),0(R3)                                           
*                                                                               
         OC    3(2,R3),3(R3)       DATE                                         
         BNZ   *+14                                                             
         MVC   SONTOBOX,0(R3)      NO DATE- THIS IS 'TO' MEMBER ID              
         B     DS20                         AND THIS IS LAST ID                 
                                                                                
         LA    R3,L'RCONSONM(R3)   BUMP TO NEXT MEMBER                          
         LA    R2,L'RCONSONM(R2)                                                
         BCT   R1,DS10                                                          
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
DS20     DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A5'        GET SONNET COMMENTS                          
         USING RCONSCEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   DS024                                                            
*                                                                               
         LA    R3,SONCOM1                                                       
DS022    DS    0H                                                               
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-(RCONSCOV+1)    SUBTRACT OVERHEAD  +1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RCONSCCM   COPY COMMENT                                  
*                                                                               
         LA    R3,SONCOM2                                                       
         BAS   RE,NEXTEL                                                        
         BE    DS022                                                            
         DROP  R6                                                               
*                                                                               
DS024    DS    0H                                                               
*                                                                               
DSX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************                            
         EJECT                                                                  
GETADVN  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RADVREC,RE                                                       
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   AIO,AIO2            READ ADV HEADER INTO AIO2                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         MVC   WORK(20),RADVNAME     ADVERTISER NAME                            
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
                                                                                
*                                                                               
GETAGYN  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RAGYREC,RE                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),RCONKAGY           AGY/OFF                           
         MVC   RAGYKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   AIO,AIO2            READ HEADER INTO AIO2                        
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         MVC   WORK(20),RAGYNAM1    AGENCY NAME                                 
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
                                                                                
*                                                                               
GETOFFN  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING ROFFREC,RE                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,RCONKOFF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   AIO,AIO2            READ INTO AIO2                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING ROFFREC,R6                                                       
         MVC   WORK(20),ROFFNAME     OFFICE NAME                                
         MVC   AIO,AIO1            RESET AIO AREA                               
         B     XIT                                                              
                                                                                
*                                                                               
GETSLSN  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RSALREC,RE                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,RCONSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   AIO,AIO2            READ INTO AIO2                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         MVC   WORK(20),RSALNAME     SALES NAME                                 
         MVC   AIO,AIO1            RESET AIO AREA                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* GET PRODCUT NAME                                                              
* RETURNS NAME IN WORK                                                          
GETPRDN  NTR1                                                                   
         XC    WORK(20),WORK                                                    
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONPRD,X'40'       IF RCONPRD=X'40'                             
         BE    GP22                FREE FORM PROD ENTERED                       
                                                                                
         XC    KEY,KEY             VALID PROD CODE                              
         LA    RE,KEY              READ PROD HEADER FOR PROD NAME               
         USING RPRDREC,RE                                                       
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKREP,AGENCY                                                  
         MVC   RPRDKPRD,RCONPRD                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GP24                                                             
         MVC   AIO,AIO2            PRODUCT RECORD INTO AIO2 AREA                
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         MVC   WORK(20),RPRDNAME     MOVE IN NAME FROM PRODUCT RECORD           
         MVC   AIO,AIO1                                                         
         B     GP24                                                             
         DROP  RE                                                               
                                                                                
* GET FREEFORM PRODUCT                                                          
GP22     DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   GP24                                                             
         USING RCONEXEL,R6                                                      
         MVC   WORK(20),RCONEXPR     PRODUCT                                    
GP24     B     XIT                                                              
         EJECT                                                                  
***********************************************                                 
* PACK - R2 HAS ADDRESS OF HEADER                                               
***********************************************                                 
PACK     NTR1                                                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
         EJECT                                                                  
***********************************************                                 
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
***********************************************                                 
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
*                                                                               
CS021    CLI   0(R2),9                                                          
         BNH   CSX                                                              
         TM    1(R2),X'20'         PROTECTED?                                   
         BNO   CS010                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     CS021                                                            
CSX      LA    R2,SOMHISTH         CLEAR THIS PROTECTED AREAS                   
         LA    R3,SOMHENDH                                                      
CSLOOP   ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,CSXC                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,9(R1,R2)                                                      
         CR    R2,R3                                                            
         BNH   CSLOOP                                                           
CSXX     B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
***********************************************                                 
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
***********************************************                                 
INVAERR  MVI   ERROR,INVACT                                                     
         GOTO1 ERREX                                                            
***********************************************                                 
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
***********************************************                                 
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
***********************************************                                 
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************                                 
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESUNFFD                                                                      
***********************************************                                 
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESUNFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESUNF1D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESUNF2D                                                       
         EJECT                                                                  
       ++INCLUDE RESUNWRK                                                       
***********************************************                                 
         ORG   SONLOCAL                                                         
SVBOXID  DS    CL(L'RBOXIDCD)                                                   
SVKEY2   DS    CL27                                                             
LISTKSV  DS    CL27                                                             
KEYTYPE  DS    X                   K SONNET PASSIVE KEY TYPE 8F/9F              
SONTOBOX DS    CL(L'RBOXIDCD)      SONNET TO MEM ID - APPEARS IN                
*                                  'FROM BOX' ON MAINT SCREEN                   
SONCOM1  DS    CL(L'SOMCOM1)                                                    
SONCOM2  DS    CL(L'SOMCOM2)                                                    
SONBLK   DS    CL600               10*(3-MEMID,2-DATE,1-APPROVED)               
         PRINT OFF                                                              
CONRECS  DSECT                                                                  
       ++INCLUDE REGENCON                                                       
STARECS  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
ADVRECS  DSECT                                                                  
       ++INCLUDE REGENADV                                                       
AGYRECS  DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
OFFRECS  DSECT                                                                  
       ++INCLUDE REGENOFF                                                       
SLSRECS  DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
BOXRECS  DSECT                                                                  
       ++INCLUDE REGENBOX                                                       
PRORECS  DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RESUN01   05/01/02'                                      
         END                                                                    
