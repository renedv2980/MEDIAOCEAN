*          DATA SET RESFM45    AT LEVEL 153 AS OF 06/24/03                      
*PHASE T81845A                                                                  
         TITLE 'T81845 - RESFM45 - DARE/ROM EQUIV RECORDS'                      
***********************************************************************         
*                                                                     *         
*  RESFM45 (T81845) --- DARE/ROM MAINTENANCE / LIST / ETC             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 17JUN03 (BU ) DATE OF ENTRY                                         *         
* .                                                                   *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T81845   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81845*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   IOOPT,C'Y'          DO MY OWN I/O'S                              
         OI    GENSTAT1,RDUPAPPL   ALLOW READ FOR UPDATE WITH                   
*                                   ACTION LIST                                 
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VALIKEY                                                          
                                                                                
         CLI   MODE,LISTRECS       LIST REC?                                    
         BNE   EXIT                                                             
                                                                                
         TM    SCRNFLAG,ACTPROCD   IF AN ACTION WAS PROCESSED LAST,             
         BZ    LIST                DON'T REFRESH SCREEN.  LET USER SEE          
         MVI   SCRNFLAG,NEXTSCRN   WHAT WAS JUST ENTERED                        
         NI    DARAGCYH+4,X'FF'-X'20'  JUST ENSURES DISPLAY FROM TOP            
         B     ACTCHGD                                                          
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VALIKEY  DS    0H                                                               
         MVI   SCRNFLAG,SAMESCRN   WE'RE NOT SCROLLING                          
         MVI   ADDFLAG,C'N'        AND WE ARE NOT ADDING                        
         TM    DARAGCYH+4,X'20'    IF NO FIELDS WERE CHANGED                    
         BZ    VKEY0060              USER WANTS TO PAGE TO THE NEXT             
                                                                                
         MVI   SCRNFLAG,NEXTSCRN                                                
*                                                                               
         GOTO1 VALNEWCD            SCAN SCREEN FOR ADDS/CHGS                    
*                                     VALIDATE NEW CODES                        
         BZ    VKEY0010            NO ERROR                                     
         L     R2,DUB              SET A(FIELD IN ERROR)                        
         B     INVLFLD             SEND BACK ERROR MESSAGE                      
VKEY0010 EQU   *                                                                
         LA    R2,DARACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VKEY0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VKEY0040                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VKEY0040            YES, THEN SKIP IT                            
VKEY0030 EQU   *                                                                
         GOTO1 PROCACT,DMCB,(R2)   GO PROCESS ACTION                            
*                                                                               
*   TEST DUMP                                                                   
         LA    RF,2                                                             
*   TEST DUMP END                                                               
*                                                                               
         BNZ   INVLFLD                                                          
         MVI   SCRNFLAG,ACTPROCD                                                
         MVC   8(3,R2),=C'***'     THIS MEANS THIS ROW WAS SUCCESSFULLY         
         OI    6(R2),X'80'          PROCESSED                                   
                                                                                
VKEY0040 DS    0H                                                               
         LA    RF,DARLACTH-DARACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,DARFINH                                                       
         CR    R2,R1                                                            
         BL    VKEY0020                                                         
         B     VKEY0180                                                         
                                                                                
VKEY0060 DS    0H                                                               
         OI    DARAGCYH+4,X'20'    SET VALIDATED                                
*                                                                               
         LA    R2,DARAGCYH         A(AGENCY CODE)                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         XC    DARKEY,DARKEY                                                    
         XC    FIRSTKEY,FIRSTKEY                                                
         MVI   DARKEY,X'4C'                                                     
         MVC   DARKEY+16(2),AGENCY                                              
         MVC   DARKEY+18(5),DARAGCY INSERT AGENCY CODE                          
         OC    DARKEY+18(5),SPACES                                              
         GOTO1 SHOWNAME            DISPLAY NAME OF CHANGED CODE                 
VKEY0180 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE REP                                                                  
* INPUT  - R2 POINTS TO REP FIELD HEADER                                        
* OUTPUT - SRCEREP CONTAINS TWO-CHARACTER POWER CODE OF SOURCE REP.             
***********************************************************************         
VALIREP  NTR1                                                                   
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         LA    RF,DARAGCYH         A(SOURCE REP)                                
         ZIC   RE,5(RF)            L(SOURCE REP INPUT)                          
         LTR   RE,RE                                                            
         BZ    VREP0080                                                         
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,VREP0800         MOVE BY LENGTH                               
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                      
         CLI   8(R1),0             FOUND?                                       
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
VREP0020 EQU   *                                                                
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   VREP0030            NO                                           
         MVC   SRCEREP,2(R1)       YES - SAVE 2-CHAR REP ID                     
         B     VREP0060            FINISHED                                     
VREP0030 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VREP0020            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
VREP0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
VREP0800 MVC   WORK+15(0),DARAGCY  LOAD SOURCE REP BY LENGTH                    
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LIST     DS    0H                                                               
         TWAXC DARACTH,PROT=Y                                                   
                                                                                
         LA    RF,DARCODEH         SET ALL DDS CODE FIELDS                      
                                                                                
LIST10   DS    0H                                                               
         XC    8(L'DARCODE,RF),8(RF)                                            
         NI    6(RF),X'FF'-X'20'   DEFAULT TO UNPROTECT                         
         LA    RE,DARLCDEH-DARCODEH                                             
         AR    RF,RE                                                            
         LA    RE,DARFINH                                                       
         CR    RF,RE                                                            
         BNH   LIST10                                                           
         CLI   ADDFLAG,C'Y'        IF USER WANTS TO ADD                         
         BNE   LIST20               CLEAR SCREEN AND EXIT W/MSG                 
         LA    R2,DARACTH                                                       
         MVI   ADDFLAG,C'N'                                                     
         B     GOADD                                                            
LIST20   DS    0H                                                               
         LA    R2,DARSTRTH                                                      
         TM    SCRNFLAG,NEXTSCRN   USER JUST PRESSED ENTER?                     
         BZ    LIST30              W/O CHANGING THE KEY FIELDS                  
         MVC   KEY,SAVEKEY         YES, DISPLAY NEXT PAGE OF CONTRACTS          
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'RDROKEY),FIRSTKEY                                      
         BE    LIST40              DON'T DO A SEQ IF DISPLAY FROM BEG           
         B     LIST150                                                          
LIST30   DS    0H                                                               
         MVC   KEY,DARKEY                                                       
         MVC   FIRSTKEY,DARKEY                                                  
         GOTO1 HIGH                                                             
LIST40   DS    0H                                                               
         CLC   KEY(23),KEYSAVE     SAME KEY THRU SCREEN AGENCY?                 
         BNE   LIST200             NO                                           
*                                                                               
LIST100  DS    0H                                                               
         GOTO1 GETREC              YES - RETRIEVE RECORD                        
         BAS   RE,DISLINE                                                       
         LA    RF,DARFINH          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   LISTX                                                            
                                                                                
LIST150  DS    0H                                                               
         GOTO1 SEQ                                                              
         B     LIST40                                                           
                                                                                
LIST200  DS    0H                  WE'VE HIT THE LAST RECORD                    
         MVC   SAVEKEY,FIRSTKEY                                                 
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         LA    R2,DARAGCYH         PUT CURSOR HERE                              
         B     ENDLIST                                                          
                                                                                
LISTX    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTLIST                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LINE OF RECORD                                                      
***********************************************************************         
DISLINE  NTR1                                                                   
         MVC   TEMPKEY,KEY         SO WE CAN RESTORE AT END OF ROUTINE          
                                                                                
         L     R6,AIO                                                           
         USING RDROREC,R6                                                       
         MVC   SAVEEQIV,RDROEQIV   SAVE EQUIVALENCY CODE                        
         MVC   8(5,R2),RDROKOAD    INSERT ORIGINAL ADV CODE                     
         OI    4(R2),X'20'         SET PREV VALID ON THIS FIELD TO              
*                                     PREVENT MODIFICATIONS                     
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
                                                                                
         MVC   8(4,R2),RDROEQIV                                                 
                                                                                
         MVC   WORK(20),=C'* RECORD NOT FOUND *'                                
                                                                                
         GOTO1 VADV                EXPANDED ADVERTISER NAME                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(13,R2),WORK                                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,RDRODATE),(5,8(R2))                               
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,RDRODCHG),(5,8(R2))                               
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(8,R2),RDROLUID                                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   KEY,TEMPKEY         RESTORE LIST SEQ ORDER                       
         GOTO1 HIGH                                                             
                                                                                
         XIT1  REGS=(R2)           R2 POINTS TO THE NEXT LINE                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT IN ACTION COLUMN                                                
* P1 HAS FIELD HEADER                                                           
***********************************************************************         
PROCACT  NTR1                                                                   
         L     R2,0(R1)                                                         
         CLC   =C'ADD',8(R2)       ACTION ADD                                   
         BE    PROCADD                                                          
         CLI   8(R2),C'A'                                                       
         BE    PROCADD                                                          
                                                                                
         CLC   =C'CHA',8(R2)       ACTION CHANGE                                
         BE    PROCGET                                                          
         CLI   8(R2),C'C'                                                       
         BE    PROCGET                                                          
                                                                                
***      CLC   =C'DEL',8(R2)       ACTION DELETE                                
***      BE    PROCGET                                                          
                                                                                
***      CLC   =C'RES',8(R2)       ACTION RESTORE                               
***      BE    PROCGET                                                          
         B     NO                  INVALID ACTION                               
         EJECT                                                                  
*                                                                               
PROCADD  DS    0H                  PROCESS ACTION ADD                           
         MVC   AIO,AIO2            SET AIO AREA = AIO2                          
         L     R6,AIO2             USE IO2                                      
         XC    0(256,R6),0(R6)                                                  
*                                                                               
         MVI   0(R6),X'4C'                                                      
         MVC   16(2,R6),AGENCY                                                  
         MVC   18(5,R6),DARAGCY                                                 
         OC    18(5,R6),SPACES    SPACE PAD                                     
*                                                                               
         MVC   27(2,R6),=X'0042'  34 FOR KEY/CTRL + 32 FOR ELT                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING RDROELEM,R5                                                      
         MVI   RDROCODE,X'01'                                                   
         MVI   RDROELLN,X'20'      SET LEN                                      
         LR    R3,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               A(AGENCY ADV CODE FLD)                       
                                                                                
*                                                                               
*   CANNOT VALIDATE THE AGENCY ADVERTISER CODE.  DON'T HAVE ACCESS              
*        TO ANY SUCH INFORMATION                                                
*                                                                               
         MVC   23(4,R6),8(R2)      INSERT AGENCY ADV CODE                       
         OC    23(4,R6),SPACES     SPACE PAD                                    
PADD70   DS    0H                                                               
         MVC   KEY,0(R6)                                                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PADD80                                                           
         TM    KEY+27,X'80'                                                     
         BO    DELRECEX            DELETED RECORD EXISTS                        
         B     RECONFIL            RECORD ALREADY ON FILE                       
                                                                                
PADD80   DS    0H                                                               
         LR    R2,R3                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               A(AGENCY ADV CODE FIELD)                     
         ZIC   R0,0(R2)            BUMP TO THE REP ADV CODE FIELD               
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),4             UP TO LENGTH OF 4                            
         BH    INVLFLD                                                          
*                                                                               
         MVC   RDROEQIV,8(R2)                                                   
         OC    RDROEQIV,SPACES     SPACE PAD                                    
         MVC   SAVEEQIV,RDROEQIV                                                
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   RDROLUID,FASYM      INSERT LUID                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,RDRODATE)                                
         MVC   RDRODCHG,RDRODATE                                                
*                                                                               
         MVC   34(32,R6),ELEM      ADD ELEMENT TO RECORD                        
         GOTO1 ADDREC                                                           
         TM    DMCB+8,X'20'        RECORD ALREADY EXISTS                        
         BO    INVLFLD                                                          
*                                                                               
*   NOW GET NEW ADVERTISER NAME                                                 
*                                                                               
         GOTO1 VADV                EXPANDED ADVERTISER NAME                     
         ZIC   R0,0(R2)            BUMP FROM CODE TO NAME FIELD                 
         AR    R2,R0                                                            
         MVC   8(13,R2),WORK                                                    
         OI    6(R2),X'80'         SET TO TRANSMIT                              
*                                                                               
         ZIC   RF,0(R2)            BUMP TO DATE ENTERED FIELD                   
         AR    R2,RF                                                            
         GOTO1 DATCON,DMCB,(5,WORK),(5,8(R2))                                   
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         ZIC   RF,0(R2)            BUMP TO DATE CHANGED FIELD                   
         AR    R2,RF                                                            
         GOTO1 DATCON,DMCB,(5,WORK),(5,8(R2))                                   
         OI    6(R2),X'80'         SET TO TRANSMIT                              
*                                                                               
         ZIC   RF,0(R2)            BUMP TO LUID FIELD                           
         AR    R2,RF                                                            
         MVC   8(8,R2),RDROLUID    INSERT LUID                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
*                                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IO AREA                              
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
PROCGET  DS    0H                  GET RECORD FOR PROCESSING                    
         MVC   KEY,DARKEY          BUILD KEY FROM CURRENT LINE                  
                                                                                
*                                                                               
         LA    R6,KEY                                                           
         USING RDROREC,R6                                                       
         XC    RDROKOAD(4),RDROKOAD                                             
*                                  ZERO-FILL AGY ADV CODE PORTION               
         LR    R3,R2               BUMP TO DDS CODES FIELD                      
         ZIC   RF,0(R3)            BUMP TO AGENCY ADV CODE FIELD                
         AR    R3,RF                                                            
***      ZIC   RF,0(R3)                                                         
***      AR    R3,RF                                                            
         LA    RF,5                SET L(CODE FIELD) FOR AGENCY CODE            
         LA    R5,27               SET L(KEY COMPARE)                           
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,PRGE0040         MOVE CODE BY LENGTH                          
         EX    RF,PRGE0050         SPACE FILL BY LENGTH                         
         B     PRGE0060                                                         
PRGE0040 MVC   RDROKOAD(0),8(R3)                                                
PRGE0050 OC    RDROKOAD(0),SPACES  SPACE FILL SIGNIFICANT PART OF CODE          
*                                  MOVE N CHARS FROM SCREEN FIELD               
         DROP  R6                                                               
PRGE0060 EQU   *                                                                
         CLC   =C'CHA',8(R2)       CHANGE ACTION?                               
         BE    PRGE0070            YES - CHECK FOR PREV VALID                   
         CLI   8(R2),C'C'          DITTO                                        
         BNE   PRGE0080                                                         
PRGE0070 EQU   *                                                                
         ZIC   RF,0(R2)            CHANGE REQUESTED: CHECK NEXT FIELD           
         AR    RF,R2                                                            
         TM    4(RF),X'20'         PREVIOUSLY VALID SET?                        
         BNO   NOTPVAL             NO  - CAN'T BE TURNED OFF                    
PRGE0080 EQU   *                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         BCTR  R5,0                SUBTRACT 1 FOR COMPARE EX                    
         EX    R5,PRGE0100         COMPARE BY LENGTH                            
         BNE   INVLFLD                                                          
                                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         CLC   =C'CHA',8(R2)       ACTION CHANGED                               
         BE    PROCCHA                                                          
         CLI   8(R2),C'C'                                                       
         BE    PROCCHA                                                          
                                                                                
         CLC   =C'DEL',8(R2)       ACTION DELETE                                
         BE    PROCDEL                                                          
                                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BE    PROCRES                                                          
         DC    H'0'                                                             
PRGE0100 CLC   KEY(0),KEYSAVE                                                   
         EJECT                                                                  
PROCCHA  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDROREC,R6                                                       
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RDRODCHG)                                   
         LR    R3,R2                                                            
         LA    R3,DARDCHGH-DARACTH(R3)                                          
*                                  SET TO DATE CHANGED FIELD                    
         GOTO1 DATCON,DMCB,(3,RDRODCHG),(5,8(R3))                               
         OI    6(R3),X'80'         SET TO TRANSMIT                              
                                                                                
         ZIC   RF,0(R2)            BUMP TO AGY ADVERT FIELD                     
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            BUMP TO REP ADVERT FIELD                     
         AR    R2,RF                                                            
                                                                                
                                                                                
         CLI   5(R2),4             LENGTH OF 4 OR LOWER                         
         BH    INVLFLD                                                          
                                                                                
         XC    RDROEQIV,RDROEQIV                                                
         LA    RF,5                SET L(CODE FIELD) FOR AGENCY                 
         BCTR  RF,0                                                             
         EX    RF,PRCH0040         FILL MAJOR CODE SPACE IN KEY                 
         ZIC   RF,5(R2)            GET FIELD INPUT LENGTH                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RDROEQIV(0),8(R2)   MOVE BY INPUT LENGTH                         
         MVC   SAVEEQIV,RDROEQIV   SET UP FOR VADV CALL                         
         B     PCHA50                                                           
PRCH0040 MVC   RDROEQIV(0),SPACES                                               
                                                                                
PCHA50   DS    0H                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   RDROLUID,FASYM      INSERT LUID OF CHANGE                        
         MVC   SAVELUID,FASYM      SAVE   LUID OF CHANGE                        
         DROP  R6,RF                                                            
*                                                                               
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
*   NOW GET NEW ADVERTISER NAME                                                 
*                                                                               
         GOTO1 VADV                EXPANDED ADVERTISER NAME                     
         ZIC   R0,0(R2)            BUMP FROM CODE TO NAME FIELD                 
         AR    R2,R0                                                            
         MVC   8(13,R2),WORK                                                    
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         LA    R2,DARLUIDH-DARNAMEH(R2)                                         
*                                  BUMP TO LUID HEADER FIELD                    
         MVC   8(8,R2),SAVELUID                                                 
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         B     YES                                                              
         EJECT                                                                  
PROCDEL  DS    0H                  PROCESS ACTION DELETE                        
         L     R6,AIO                                                           
         USING RDROREC,R6                                                       
         OI    RDROCNTL,X'80'      MARK FOR DELETION                            
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         B     YES                                                              
         EJECT                                                                  
PROCRES  DS    0H                  PROCESS ACTION RESTORE                       
         L     R6,AIO                                                           
         USING RDROREC,R6                                                       
         NI    RDROCNTL,X'FF'-X'80' RESTORE                                     
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         NI    KEY+27,X'FF'-X'80'  RESTORE                                      
         GOTO1 WRITE                                                            
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*   VALNEWCD:  SCAN SCREEN.  IF CODE ENTERED, VALIDATE FOR EXISTENCE.           
*                                                                               
VALNEWCD NTR1                                                                   
         LA    R2,DARACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VNEW0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VNEW0440                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VNEW0440            YES, THEN SKIP IT                            
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               SET A(FIELD IN PROGRESS)                     
         ZIC   RF,0(R3)            STRIP LENGTH                                 
         AR    R3,RF               BUMP TO AGY ADV FIELD                        
         ZIC   RF,0(R3)            STRIP LENGTH                                 
         AR    R3,RF               BUMP TO REP ADV FIELD                        
         CLI   5(R3),4             EXCEED MAX LENGTH?                           
         BH    VNEW0900            YES                                          
         MVC   KEY+21(4),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+21(4),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0900            NO  - ERROR ENCOUNTERED                      
         B     VNEW0440            YES - CHECK NEXT FIELD                       
VNEW0440 DS    0H                                                               
         LA    RF,DARLACTH-DARACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,DARFINH                                                       
         CR    R2,R1                                                            
         BL    VNEW0020            GO BACK FOR NEXT                             
         B     VNEW0920            FINISHED                                     
VNEW0900 DS    0H                                                               
         ST    R3,DUB              SET A(FIELD IN ERROR)                        
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VNEW0990            EXIT                                         
VNEW0920 EQU   *                                                                
         SR    R0,R0                                                            
VNEW0990 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SHOWNAME:  SCAN SCREEN.  IF CODE ENTERED, VALIDATE FOR EXISTENCE.           
*                                                                               
SHOWNAME NTR1                                                                   
         LA    R2,DARACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
SNAM0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    SNAM0440                                                         
         CLC   8(3,R2),=C'***'     WAS THIS LINE PROCESSED?                     
         BNE   SNAM0440            NO  - SKIP IT                                
         MVC   8(3,R2),=C'*  '     YES - RESET IT                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               BUMP TO AGENCY ADV CODE FIELD                
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO REP ADV CODE FIELD                   
         LR    R4,R3               ALSO BUMP TO EXPANSION FIELD                 
         ZIC   RF,0(R4)            PASS TWO FIELDS                              
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(13,R4),=C'*NOT ON FILE*'                                       
*                                                                               
         CLI   5(R3),4             EXCEED MAX LENGTH?                           
         BH    SNAM0900            YES                                          
         MVC   KEY+21(4),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+21(4),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SNAM0440            NO  - USE 'DEFAULT VALUE'                    
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   8(13,R4),RADVNAME   INSERT ADVERT NAME                           
         OI    6(R4),X'80'         SET TO TRANSMIT                              
         B     SNAM0440            CHECK NEXT FIELD                             
         DROP  R6                                                               
SNAM0440 DS    0H                                                               
         LA    RF,DARLACTH-DARACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,DARFINH                                                       
         CR    R2,R1                                                            
         BL    SNAM0020            GO BACK FOR NEXT                             
         B     SNAM0920            FINISHED                                     
SNAM0900 DS    0H                                                               
         ST    R3,DUB              SET A(FIELD IN ERROR)                        
         LTR   RB,RB               SET CC NOT ZERO                              
         B     SNAM0990            EXIT                                         
SNAM0920 EQU   *                                                                
         SR    R0,R0                                                            
SNAM0990 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* RETRIEVES ADVERTISER NAME TO WORK                                             
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VADV     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKREP,AGENCY     REP                                          
         MVC   RADVKADV,SAVEEQIV   ADV                                          
         OC    RADVKADV,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
                                                                                
         GOTO1 GETREC                                                           
         MVC   WORK(20),RADVNAME                                                
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
NUMERIC  MVC   RERROR,=AL2(NOTNUM) MUST BE NUMERIC                              
         B     ERREND                                                           
                                                                                
RECONFIL MVC   RERROR,=AL2(RECEXIST) RECORD ALREADY ON FILE                     
         B     ERREND                                                           
                                                                                
DELRECEX MVC   RERROR,=AL2(DELEXIST) DELETED RECORD EXISTS                      
         B     ERREND                                                           
                                                                                
RECACTIV MVC   RERROR,=AL2(290)    CANNOT DELETE ACTIVE RECORD.                 
         B     ERREND                                                           
                                                                                
NOTPVAL  MVC   RERROR,=AL2(971)    FIELD CANNOT BE CHANGED                      
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
ACTCHGD  MVC   RERROR,=AL2(106)    RECORDS CHANGED.  PRESS ENTER TO             
         LA    R2,DARAGCYH           PROCEED                                    
         B     RINFEND                                                          
                                                                                
GOADD    MVC   RERROR,=AL2(107)    PLEASE ADD RECORDS.                          
         LA    R2,DARACTH                                                       
         B     RINFEND                                                          
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
INFEND   DS    0H                                                               
         LA    R2,DARAGCYH         PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
RINFEND  DS    0H                  USE REP INFO FILE                            
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,X'08'                                                     
         DROP  RF                                                               
                                                                                
         MVI   RMSGTYPE,C'I'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
TEMPKEY  DS    CL(L'KEY)                                                        
SRCEREP  DS    CL2                 SOURCE REP POWER CODE                        
SAVEEQIV DS    CL6                 SAVE EQUIVALENCY CODE                        
SAVELUID DS    CL8                 SAVE LUID OF RECORD                          
*                                                                               
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM82D          (OUR MAINTENANCE SCREEN OVERLAY)             
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
DARREC   DSECT                                                                  
       ++INCLUDE REGENDRO                                                       
         DSECT                                                                  
       ++INCLUDE REGENADV                                                       
         DSECT                                                                  
       ++INCLUDE FAFACTS                                                        
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
DARKEY   DS    CL(L'KEY)                                                        
SAVEKEY  DS    CL(L'KEY)                                                        
FIRSTKEY DS    CL(L'KEY)                                                        
SCRNFLAG DS    X                                                                
NEXTSCRN EQU   X'10'                                                            
SAMESCRN EQU   X'20'                                                            
ACTPROCD EQU   X'40'               ACTION WAS PROCESSED                         
ADDFLAG  DS    C                   Y=USER WANTS TO ADD, BLANK SCREEN            
FILTER   DS    X                                                                
TRAFFSYS DS    X                                                                
CODESIZE DS    X                   LEN OF CONTRACT FIELD                        
CODEOFFS DS    X                   OFFSET OF CONTRACT FIELD FROM KEY            
JDSFLDLN DS    X                   JDS EOP CODE FIELD LENGTH                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'153RESFM45   06/24/03'                                      
         END                                                                    
