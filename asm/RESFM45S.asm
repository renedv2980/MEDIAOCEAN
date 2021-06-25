*          DATA SET RESFM45S   AT LEVEL 093 AS OF 12/14/99                      
*PHASE T81831A                                                                  
         TITLE 'T81831 - RESFM31 - TKO EQUIVALENCY RECORDS'                     
***********************************************************************         
*                                                                     *         
*  RESFM31 (T81831) --- TKO MAINTENANCE/LIST                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 03MAR99 (SKU) VALIREP BUG FIX                                       *         
* 15AUG97 (BU ) DATE OF ENTRY                                         *         
* .                                                                   *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T81831   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81831*,R7,RR=R3                                              
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
         NI    TKOSRCEH+4,X'FF'-X'20'  JUST ENSURES DISPLAY FROM TOP            
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
                                                                                
         TM    TKOSRCEH+4,X'20'    IF NO FIELDS WERE CHANGED                    
         BZ    VKEY0060              USER WANTS TO PAGE TO THE NEXT             
**       TM    TKOSTATH+4,X'20'      SCREEN                                     
**       BZ    VKEY0060                                                         
         TM    TKOKADVH+4,X'20'                                                 
         BZ    VKEY0060                                                         
         TM    TKOKAGYH+4,X'20'                                                 
         BZ    VKEY0060                                                         
         TM    TKOKSALH+4,X'20'                                                 
         BZ    VKEY0060                                                         
         TM    TKOKTYPH+4,X'20'                                                 
         BZ    VKEY0060                                                         
                                                                                
         TM    TKOSRCEH+4,X'80'    IF FIELDS WERE CHANGED BUT CHANGED           
         BO    VKEY0060              TO THE SAME DATA, USER WANTS TO            
**       TM    TKOSTATH+4,X'80'      UPDT FROM THE BEGINNING                    
**       BO    VKEY0060                                                         
         TM    TKOKADVH+4,X'80'                                                 
         BO    VKEY0060                                                         
         TM    TKOKAGYH+4,X'80'                                                 
         BO    VKEY0060                                                         
         TM    TKOKSALH+4,X'80'                                                 
         BO    VKEY0060                                                         
         TM    TKOKTYPH+4,X'80'                                                 
         BO    VKEY0060                                                         
                                                                                
         MVI   SCRNFLAG,NEXTSCRN                                                
*                                                                               
         GOTO1 VALNEWCD            SCAN SCREEN FOR ADDS/CHGS                    
*                                     VALIDATE NEW CODES                        
         BZ    VKEY0010            NO ERROR                                     
         L     R2,DUB              SET A(FIELD IN ERROR)                        
         B     INVLFLD             SEND BACK ERROR MESSAGE                      
VKEY0010 EQU   *                                                                
         LA    R2,TKOACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VKEY0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VKEY0040                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VKEY0040            YES, THEN SKIP IT                            
                                                                                
         GOTO1 PROCACT,DMCB,(R2)   GO PROCESS ACTION                            
         BNZ   INVLFLD                                                          
         MVI   SCRNFLAG,ACTPROCD                                                
         MVC   8(3,R2),=C'***'     THIS MEANS THIS ROW WAS SUCCESSFULLY         
         OI    6(R2),X'80'          PROCESSED                                   
                                                                                
VKEY0040 DS    0H                                                               
         LA    RF,TKOLACTH-TKOACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,TKOFINH                                                       
         CR    R2,R1                                                            
         BL    VKEY0020                                                         
         B     VKEY0180                                                         
                                                                                
VKEY0060 DS    0H                                                               
         OI    TKOSRCEH+4,X'20'    SET VALIDATED                                
**       OI    TKOSTATH+4,X'20'    SET VALIDATED                                
         OI    TKOKADVH+4,X'20'    SET VALIDATED                                
         OI    TKOKAGYH+4,X'20'    SET VALIDATED                                
         OI    TKOKSALH+4,X'20'    SET VALIDATED                                
         OI    TKOKTYPH+4,X'20'    SET VALIDATED                                
                                                                                
         LA    R2,TKOSRCEH         A(SOURCE REP)                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIREP                                                          
         BNZ   INVLFLD             ERROR:  REP NOT FOUND                        
*                                                                               
         XC    TKOKEY,TKOKEY                                                    
         XC    FIRSTKEY,FIRSTKEY                                                
         MVI   FILTER,0                                                         
                                                                                
         LA    R2,TKOKADVH         POINT HERE IF ERROR                          
                                                                                
         CLI   TKOKADVH+5,0        ONE AND ONLY ONE OF THESE FIELDS             
         BE    VKEY0120            MUST HAVE AN INPUT                           
                                                                                
         CLI   TKOKADV,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   TKOKADV,C'A'                                                     
                                                                                
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   TKOHFLD(3),=C'ADV'  SOFT SCREEN HEADING                          
                                                                                
         MVI   TKOKEY,X'1F'                                                     
         MVC   TKOKEY+16(2),AGENCY                                              
         MVC   TKOKEY+18(2),SRCEREP                                             
         MVI   TKOKEY+20,2         SET KEY TO 'ADV'                             
         MVC   TKOKEY+21(4),TKOKADV                                             
                                                                                
VKEY0120 DS    0H                                                               
         CLI   TKOKAGYH+5,0                                                     
         BE    VKEY0140                                                         
                                                                                
         CLI   TKOKAGY,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   TKOKAGY,C'A'                                                     
                                                                                
         LA    R2,TKOKAGYH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   TKOHFLD(3),=C'AGY'  SOFT SCREEN HEADING                          
                                                                                
         MVI   TKOKEY,X'1F'                                                     
         MVC   TKOKEY+16(2),AGENCY                                              
         MVC   TKOKEY+18(2),SRCEREP                                             
         MVI   TKOKEY+20,1         SET KEY TO 'AGY'                             
         MVC   TKOKEY+21(6),TKOKAGY                                             
                                                                                
VKEY0140 DS    0H                                                               
         CLI   TKOKSALH+5,0                                                     
         BE    VKEY0160                                                         
                                                                                
         CLI   TKOKSAL,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   TKOKSAL,C'A'                                                     
                                                                                
         LA    R2,TKOKSALH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   TKOHFLD(3),=C'SAL'  SOFT SCREEN HEADING                          
                                                                                
         MVI   TKOKEY,X'1F'                                                     
         MVC   TKOKEY+16(2),AGENCY                                              
         MVC   TKOKEY+18(2),SRCEREP                                             
         MVI   TKOKEY+20,3         SET KEY TO 'S/P'                             
         MVC   TKOKEY+21(3),TKOKSAL                                             
                                                                                
VKEY0160 DS    0H                                                               
         CLI   TKOKTYPH+5,0                                                     
         BE    VKEY0180                                                         
                                                                                
         CLI   TKOKTYP,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   TKOKTYP,C'A'                                                     
                                                                                
         LA    R2,TKOKTYPH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   TKOHFLD(3),=C'TYP'  SOFT SCREEN HEADING                          
                                                                                
         MVI   TKOKEY,X'1F'                                                     
         MVC   TKOKEY+16(2),AGENCY                                              
         MVC   TKOKEY+18(2),SRCEREP                                             
         MVI   TKOKEY+20,4         SET KEY TO 'CONTYPE'                         
         CLI   TKOKTYP,C'?'        START WITH FIRST RECORD?                     
         BE    VKEY0180            YES                                          
         MVC   TKOKEY+21(1),TKOKTYP                                             
                                                                                
VKEY0180 EQU   *                                                                
         CLI   FILTER,0            MUST HAVE AT LEAST ONE FIELD                 
         BE    MISSFLD                                                          
         CLI   FILTER,1            CAN ONLY FILTER ON ONE FIELD                 
         BH    INVLFLD                                                          
         GOTO1 SHOWNAME            DISPLAY NAME OF CHANGED CODE                 
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
         LA    RF,TKOSRCEH         A(SOURCE REP)                                
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
VREP0800 MVC   WORK+15(0),TKOSRCE  LOAD SOURCE REP BY LENGTH                    
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LIST     DS    0H                                                               
         TWAXC TKOACTH,PROT=Y                                                   
                                                                                
         LA    RF,TKOCODEH         SET ALL DDS CODE FIELDS                      
                                                                                
LIST10   DS    0H                                                               
         XC    8(L'TKOCODE,RF),8(RF)                                            
         NI    6(RF),X'FF'-X'20'   DEFAULT TO UNPROTECT                         
         LA    RE,TKOLCDEH-TKOCODEH                                             
         AR    RF,RE                                                            
         LA    RE,TKOFINH                                                       
         CR    RF,RE                                                            
         BNH   LIST10                                                           
                                                                                
         CLI   ADDFLAG,C'Y'        IF USER WANTS TO ADD                         
         BNE   LIST20               CLEAR SCREEN AND EXIT W/MSG                 
         LA    R2,TKOACTH                                                       
         MVI   ADDFLAG,C'N'                                                     
         B     GOADD                                                            
                                                                                
LIST20   DS    0H                                                               
         LA    R2,TKOSTRTH                                                      
                                                                                
         TM    SCRNFLAG,NEXTSCRN   USER JUST PRESSED ENTER?                     
         BZ    LIST30              W/O CHANGING THE KEY FIELDS                  
         MVC   KEY,SAVEKEY         YES, DISPLAY NEXT PAGE OF CONTRACTS          
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'RTKOKEY),FIRSTKEY                                      
         BE    LIST40              DON'T DO A SEQ IF DISPLAY FROM BEG           
         B     LIST150                                                          
                                                                                
LIST30   DS    0H                                                               
         MVC   KEY,TKOKEY                                                       
         MVC   FIRSTKEY,TKOKEY                                                  
         GOTO1 HIGH                                                             
                                                                                
LIST40   DS    0H                                                               
         CLC   KEY(21),KEYSAVE     SAME KEY THRU TYPE?                          
         BNE   LIST200             NO                                           
*                                                                               
LIST100  DS    0H                                                               
         GOTO1 GETREC              YES - RETRIEVE RECORD                        
         BAS   RE,DISLINE                                                       
         LA    RF,TKOFINH          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   LISTX                                                            
                                                                                
LIST150  DS    0H                                                               
         GOTO1 SEQ                                                              
         B     LIST40                                                           
                                                                                
LIST200  DS    0H                  WE'VE HIT THE LAST RECORD                    
         MVC   SAVEKEY,FIRSTKEY                                                 
         MVC   TKOLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         LA    R2,TKOSRCEH         PUT CURSOR HERE                              
         B     ENDLIST                                                          
                                                                                
LISTX    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   TKOLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTLIST                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LINE OF RECORD                                                      
***********************************************************************         
DISLINE  NTR1                                                                   
         MVC   TEMPKEY,KEY         SO WE CAN RESTORE AT END OF ROUTINE          
                                                                                
         L     R6,AIO                                                           
         USING RTKOREC,R6                                                       
         CLI   RTKOKRTP,3          SALESPERSON CODE?                            
         BNE   DILI0020            NO  - SHOW ALL 6 CHARS                       
         MVC   8(3,R2),RTKOEQIV    YES - ONLY SHOW S/P CODE                     
         B     DILI0040                                                         
DILI0020 EQU   *                                                                
         MVC   8(6,R2),RTKOEQIV    INSERT EQUIVALENCY                           
DILI0040 EQU   *                                                                
                                                                                
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
                                                                                
         MVC   SAVEEQIV,RTKOEQIV   SAVE EQUIVALENCY CODE                        
         MVC   8(6,R2),RTKOKCOD    INSERT ORIGINAL (SOURCE) CODE                
         OI    6(R2),X'20'         PROTECT THIS FIELD, USER CAN'T CHG           
                                                                                
         MVC   WORK(20),=C'* RECORD NOT FOUND *'                                
                                                                                
         CLI   RTKOKRTP,2          ADVERT RECORD?                               
         BNE   DILI0060                                                         
         GOTO1 VADV                EXPANDED ADVERTISER NAME                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DILI0120                                                         
                                                                                
DILI0060 DS    0H                                                               
         CLI   RTKOKRTP,1          AGENCY RECORD?                               
         BNE   DILI0080                                                         
         GOTO1 VAGY                EXPANDED AGENCY NAME                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DILI0120                                                         
                                                                                
DILI0080 DS    0H                                                               
         CLI   RTKOKRTP,3          S/P RECORD?                                  
         BNE   DILI0100                                                         
         GOTO1 VSAL                EXPANDED SALESPERSON NAME                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DILI0120                                                         
                                                                                
DILI0100 DS    0H                                                               
         CLI   RTKOKRTP,4          CONTRACT TYPE RECORD?                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VCTYP               EXPANDED CONTYPE NAME                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DILI0120                                                         
                                                                                
DILI0120 DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,RTKODATE),(5,8(R2))                               
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,RTKODCHG),(5,8(R2))                               
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
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
                                                                                
*                                                                               
*   ADD IS DEACTIVATED.  ADDS REQUIRE CROSS-FILE VALIDATION, AND THAT           
*        THE SCREENS BE MODIFIED TO ALLOW ENTRY OF NEW SOURCE-SIDE              
*        CODES.                                                                 
***      CLC   =C'ADD',8(R2)       ACTION ADD                                   
***      BE    PROCADD                                                          
***      CLI   8(R2),C'A'                                                       
***      BE    PROCADD                                                          
                                                                                
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
         L     R6,AIO2             USE IO2                                      
         XC    0(256,R6),0(R6)                                                  
         MVC   0(L'RTKOKEY,R6),TKOKEY                                           
         MVC   27(L'RTKOLEN,R6),=X'0022'                                        
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING RTKOELEM,R5                                                      
         MVI   RTKOCODE,X'01'                                                   
***      MVI   REOPELLN,16                                                      
***      GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
                                                                                
         LR    R3,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO THE EOP CODE FIELD                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO THE DDS FIELD                       
                                                                                
*        CLI   RTKOKRTP,2          ADVERTISER?                                  
*        BNE   PADD10                                                           
         GOTO1 VADV                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD10   DS    0H                                                               
*        CLI   RTKOKRTP,1          AGENCY?                                      
*        BNE   PADD20                                                           
         GOTO1 VAGY                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD20   DS    0H                                                               
*        CLI   RTKOKRTP,3          S/P?                                         
*        BNE   PADD30                                                           
         GOTO1 VSAL                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD30   DS    0H                                                               
         GOTO1 VCTYP               CONTRACT TYPE!                               
         BNZ   INVLFLD                                                          
                                                                                
PADD40   DS    0H                                                               
         MVC   AIO,AIO2            VADV/VAGY/VOFF/VSAL USES AIO3                
*                                    SO CHANGE IT BACK TO AIO2                  
         ZIC   RF,CODEOFFS         BUILD LAST FIELD IN KEY                      
         AR    RF,R6               FIELD OFFSET IN KEY                          
         ZIC   R1,CODESIZE                                                      
         BCTR  R1,0                                                             
         EX    R1,PADD50                                                        
         EX    R1,PADD60                                                        
         B     PADD70                                                           
PADD50   MVC   0(0,RF),8(R2)       ADV/AGY/OFF/SAL                              
PADD60   OC    0(0,RF),SPACES      SPACE PAD                                    
                                                                                
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
         AR    R2,R0               POINT TO THE EOP CODE FIELD                  
                                                                                
                                                                                
         CLI   5(R2),6             UP TO LENGTH OF 6                            
         BH    INVLFLD                                                          
         TM    4(R2),X'08'         NUMERIC INPUTS ONLY                          
         BZ    INVLFLD                                                          
                                                                                
***      XC    REOPEQUV,REOPEQUV                                                
***      MVC   REOPEQUV(6),=6C'0'                                               
***      LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
         B     PADD140                                                          
                                                                                
PADD90   DS    0H                  FOR JDS                                      
         CLC   JDSFLDLN,5(R2)      CHECK LENGTH                                 
         BL    INVLFLD                                                          
                                                                                
         CLI   TKOKEY,X'1C'        IF AGENCY                                    
         BE    PADD120                                                          
         CLI   TKOKEY,X'1E'        IF SALESPERSON                               
         BE    PADD120                                                          
                                                                                
         ZIC   R1,JDSFLDLN                                                      
         BCTR  R1,0                                                             
***      EX    R1,PADD100                                                       
***      EX    R1,PADD110                                                       
         B     PADD140                                                          
***PADD100  MVC   REOPEQUV(0),8(R2)                                             
***PADD110  OC    REOPEQUV(0),SPACES                                            
                                                                                
PADD120  DS    0H                                                               
         TM    4(R2),X'08'         NUMERIC??                                    
         BNZ   PADD125                                                          
***      MVC   REOPEQUV(6),8(R2)                                                
         B     PADD140                                                          
                                                                                
PADD125  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,JDSFLDLN                                                      
***      LA    RE,REOPEQUV                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
***      EX    RF,PADD130                                                       
***      EX    R1,PADD135                                                       
         B     PADD140                                                          
***PADD130  MVC   REOPEQUV(0),=6C'0'                                            
***PADD135  MVC   0(0,RE),8(R2)                                                 
                                                                                
PADD140  DS    0H                                                               
         MVC   AIO,AIO2                                                         
                                                                                
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         TM    DMCB+8,X'20'        RECORD ALREADY EXISTS                        
         BO    INVLFLD                                                          
                                                                                
         MVC   AIO,AIO1            RESTORE IO AREA                              
                                                                                
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
PROCGET  DS    0H                  GET RECORD FOR PROCESSING                    
         MVC   KEY,TKOKEY          BUILD KEY FROM CURRENT LINE                  
                                                                                
         LR    R3,R2               BUMP TO DDS CODES FIELD                      
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         LA    R6,KEY                                                           
         USING RTKOREC,R6                                                       
         XC    RTKOKCOD(6),RTKOKCOD                                             
*                                  ZERO-FILL CODE AREA                          
         LA    RF,6                SET L(CODE FIELD) FOR AGENCY                 
         LA    R5,27               SET L(KEY COMPARE)                           
         CLI   RTKOKRTP,1          AGENCY RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,4                SET L(CODE FIELD) FOR ADVERT                 
         LA    R5,25               SET L(KEY COMPARE)                           
         CLI   RTKOKRTP,2          ADVERT RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,3                SET L(CODE FIELD) FOR S/P                    
         LA    R5,24               SET L(KEY COMPARE)                           
         CLI   RTKOKRTP,3          S/P    RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,1                SET L(CODE FIELD) FOR CONTYPE                
         LA    R5,22               SET L(KEY COMPARE)                           
PRGE0020 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,PRGE0040         MOVE CODE BY LENGTH                          
         EX    RF,PRGE0050         SPACE FILL BY LENGTH                         
         B     PRGE0060                                                         
PRGE0040 MVC   RTKOKCOD(0),8(R3)                                                
PRGE0050 OC    RTKOKCOD(0),SPACES  SPACE FILL SIGNIFICANT PART OF CODE          
*                                  MOVE N CHARS FROM SCREEN FIELD               
         DROP  R6                                                               
PRGE0060 EQU   *                                                                
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
         USING RTKOREC,R6                                                       
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RTKODCHG)                                   
         LR    R3,R2                                                            
         LA    R3,TKODCHGH-TKOACTH(R3)                                          
*                                  SET TO DATE CHANGED FIELD                    
         GOTO1 DATCON,DMCB,(3,RTKODCHG),(5,8(R3))                               
         OI    6(R3),X'80'         SET TO TRANSMIT                              
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
                                                                                
                                                                                
         CLI   5(R2),6             LENGTH OF 6 OR LOWER                         
         BH    INVLFLD                                                          
                                                                                
         XC    RTKOEQIV,RTKOEQIV                                                
         LA    RF,6                SET L(CODE FIELD) FOR AGENCY                 
         CLI   RTKOKRTP,1          AGENCY RECORD?                               
         BE    PRCH0020                                                         
         LA    RF,4                SET L(CODE FIELD) FOR ADVERT                 
         CLI   RTKOKRTP,2          ADVERT RECORD?                               
         BE    PRCH0020                                                         
         LA    RF,3                SET L(CODE FIELD) FOR S/P                    
         CLI   RTKOKRTP,3          S/P    RECORD?                               
         BE    PRCH0020                                                         
         LA    RF,1                SET L(CODE FIELD) FOR CONTYPE                
PRCH0020 EQU   *                                                                
         BCTR  RF,0                                                             
         EX    RF,PRCH0040         FILL MAJOR CODE SPACE IN KEY                 
         CLI   RTKOKRTP,3          S/P    RECORD?                               
         BNE   PRCH0060            NO  - EQUIV CODE SET                         
*                                  YES - NEED TO ADD S/P OFFICE CODE            
         LR    R4,R2               BUMP TO SOFF FIELD                           
         LA    R4,TKOSOFFH-TKOSTRTH(R4)                                         
         MVC   RTKOEQIV+3(2),8(R4) MOVE OUT S/P OFFICE CODE                     
*                                     (STORED 'HIDDEN' ON SCREEN)               
         B     PRCH0060                                                         
PRCH0040 MVC   RTKOEQIV(0),SPACES                                               
PRCH0060 EQU   *                                                                
         ZIC   RF,5(R2)            GET FIELD INPUT LENGTH                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RTKOEQIV(0),8(R2)   MOVE BY INPUT LENGTH                         
         B     PCHA50                                                           
                                                                                
         DROP  R6                                                               
                                                                                
PCHA50   DS    0H                                                               
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         EJECT                                                                  
PROCDEL  DS    0H                  PROCESS ACTION DELETE                        
         L     R6,AIO                                                           
         USING RTKOREC,R6                                                       
         OI    RTKOCNTL,X'80'      MARK FOR DELETION                            
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         B     YES                                                              
         EJECT                                                                  
PROCRES  DS    0H                  PROCESS ACTION RESTORE                       
         L     R6,AIO                                                           
         USING RTKOREC,R6                                                       
         NI    RTKOCNTL,X'FF'-X'80' RESTORE                                     
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
         LA    R2,TKOACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VNEW0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VNEW0440                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VNEW0440            YES, THEN SKIP IT                            
         XC    KEY,KEY             CLEAR THE KEY                                
         CLC   =C'ADV',TKOHFLD     ADVERTISER?                                  
         BNE   VNEW0040            NO                                           
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),4             EXCEED MAX LENGTH?                           
         BH    VNEW0900            YES                                          
         MVC   KEY+21(4),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+21(4),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0900            NO  - ERROR ENCOUNTERED                      
         B     VNEW0440            YES - CHECK NEXT FIELD                       
VNEW0040 EQU   *                                                                
         CLC   =C'AGY',TKOHFLD     AGENCY?                                      
         BNE   VNEW0060            NO                                           
         MVI   KEY,X'0A'           SET 'AGENCY KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         MVC   KEY+19(6),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+19(6),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0900            NO  - ERROR ENCOUNTERED                      
         B     VNEW0440            YES - CHECK NEXT FIELD                       
VNEW0060 EQU   *                                                                
         CLC   =C'SAL',TKOHFLD     SALESPERSON?                                 
         BNE   VNEW0080            NO                                           
         MVI   KEY,X'06'           SET 'S/P    KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),3             EXCEED MAX LENGTH?                           
         BH    VNEW0900            YES                                          
         MVC   KEY+22(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+24(3),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+24(3),SPACES    SET BIN ZERO TO SPACE                        
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0900            NO  - ERROR ENCOUNTERED                      
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3               BUMP TO 'SAVE S/P OFFICE'                    
         LA    R4,TKOSOFFH-TKOSTRTH(R4)                                         
         MVC   8(2,R4),RSALOFF     SAVE NEW S/P OFFICE HERE                     
         B     VNEW0440            CHECK NEXT FIELD                             
VNEW0080 EQU   *                                                                
         CLC   =C'TYP',TKOHFLD     CONTRACT TYPE?                               
         BE    VNEW0100            NO                                           
         DC    H'0'                SHOULD NEVER HAPPEN                          
VNEW0100 EQU   *                                                                
         MVI   KEY,X'32'           SET 'CONTYP KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),1             EXCEED MAX LENGTH?                           
         BH    VNEW0900            YES                                          
         MVC   KEY+24(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+26(1),8(R3)     INSERT CODE IN KEY                           
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0900            NO  - ERROR ENCOUNTERED                      
         B     VNEW0440            YES - CHECK NEXT FIELD                       
VNEW0440 DS    0H                                                               
         LA    RF,TKOLACTH-TKOACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,TKOFINH                                                       
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
         LA    R2,TKOACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
SNAM0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    SNAM0440                                                         
         CLC   8(3,R2),=C'***'     WAS THIS LINE PROCESSED?                     
         BNE   SNAM0440            NO  - SKIP IT                                
         MVC   8(3,R2),=C'*  '     YES - RESET IT                               
         XC    KEY,KEY             CLEAR THE KEY                                
         CLC   =C'ADV',TKOHFLD     ADVERTISER?                                  
         BNE   SNAM0040            NO                                           
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         LR    R4,R3               ALSO BUMP TO EXPANSION FIELD                 
         ZIC   RF,0(R4)            PASS TWO FIELDS                              
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(20,R4),=C'**CODE NOT ON FILE**'                                
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
         MVC   8(20,R4),RADVNAME   INSERT ADVERT NAME                           
         OI    6(R4),X'80'         SET TO TRANSMIT                              
         B     SNAM0440            CHECK NEXT FIELD                             
         DROP  R6                                                               
SNAM0040 EQU   *                                                                
         CLC   =C'AGY',TKOHFLD     AGENCY?                                      
         BNE   SNAM0060            NO                                           
         MVI   KEY,X'0A'           SET 'AGENCY KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         LR    R4,R3               ALSO BUMP TO EXPANSION FIELD                 
         ZIC   RF,0(R4)            PASS TWO FIELDS                              
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(20,R4),=C'**CODE NOT ON FILE**'                                
*                                                                               
         MVC   KEY+19(6),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+19(6),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SNAM0440            NO  - USE 'DEFAULT VALUE'                    
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   8(20,R4),RAGYNAM1   INSERT AGENCY NAME                           
         OI    6(R4),X'80'         SET TO TRANSMIT                              
         B     SNAM0440            YES - CHECK NEXT FIELD                       
         DROP  R6                                                               
SNAM0060 EQU   *                                                                
         CLC   =C'SAL',TKOHFLD     SALESPERSON?                                 
         BNE   SNAM0080            NO                                           
         MVI   KEY,X'06'           SET 'S/P    KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         LR    R4,R3               ALSO BUMP TO EXPANSION FIELD                 
         ZIC   RF,0(R4)            PASS TWO FIELDS                              
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(20,R4),=C'**CODE NOT ON FILE**'                                
*                                                                               
         CLI   5(R3),3             EXCEED MAX LENGTH?                           
         BH    SNAM0900            YES                                          
         MVC   KEY+22(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+24(3),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+24(3),SPACES    SET BIN ZERO TO SPACE                        
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SNAM0440            NO  - USE 'DEFAULT VALUE'                    
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   8(20,R4),RSALNAME   INSERT S/P    NAME                           
         OI    6(R4),X'80'         SET TO TRANSMIT                              
         B     SNAM0440            CHECK NEXT FIELD                             
         DROP  R6                                                               
SNAM0080 EQU   *                                                                
         CLC   =C'TYP',TKOHFLD     CONTRACT TYPE?                               
         BE    SNAM0100            NO                                           
         DC    H'0'                SHOULD NEVER HAPPEN                          
SNAM0100 EQU   *                                                                
         MVI   KEY,X'32'           SET 'CONTYP KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         LR    R4,R3               ALSO BUMP TO EXPANSION FIELD                 
         ZIC   RF,0(R4)            PASS TWO FIELDS                              
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(20,R4),=C'**CODE NOT ON FILE**'                                
*                                                                               
         CLI   5(R3),1             EXCEED MAX LENGTH?                           
         BH    SNAM0900            YES                                          
         MVC   KEY+24(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+26(1),8(R3)     INSERT CODE IN KEY                           
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SNAM0440            NO  - ERROR: USE DEFAULT                     
         L     R6,AIO                                                           
         USING RCTYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   8(20,R4),RCTYDESC   INSERT CONTYP DESCRIPTION                    
         OI    6(R4),X'80'         SET TO TRANSMIT                              
         B     SNAM0440            CHECK NEXT FIELD                             
         DROP  R6                                                               
SNAM0440 DS    0H                                                               
         LA    RF,TKOLACTH-TKOACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,TKOFINH                                                       
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
*********************************************************************           
* RETRIEVES AGENCY NAME TO WORK                                                 
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VAGY     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKREP,AGENCY     REP                                          
         MVC   RAGYKAGY(6),SAVEEQIV   AGENCY+OFF                                
         OC    RAGYKAGY(6),SPACES                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
                                                                                
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* RETRIEVES OFFICE NAME TO WORK                                                 
* R2 IS FIELD HEADER                                                            
* USES IO3 FOR TEMP IOAREA                                                      
*********************************************************************           
VCTYP    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTYKEY,R6                                                       
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,AGENCY     REP                                          
         MVC   RCTYKCTY,SAVEEQIV   OFFICE CODE                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RCTYREC,R6                                                       
         GOTO1 GETREC                                                           
         MVC   WORK(20),RCTYDESC                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* RETRIEVES SALESPERSON NAME TO WORK                                            
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VSAL     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY     REP                                          
         MVC   RSALKSAL,SAVEEQIV   SALESMAN INITIALS                            
         OC    RSALKSAL,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC                                                           
         MVC   WORK(20),RSALNAME                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
*********************************************************************           
CKACCESS NTR1                                                                   
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   YES                                                              
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
                                                                                
CKACC10  DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    YES                 YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    CKACC10                                                          
         B     NO                  ALL DONE, NO MATCH, NOT VALID                
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
                                                                                
NOTJDS   MVC   RERROR,=AL2(410)    STATION IS NOT A JDS/2000 STATION.           
         B     ERREND                                                           
                                                                                
NOTBIAS  MVC   RERROR,=AL2(411)    STATION IS NOT A BIAS STATION.               
         B     ERREND                                                           
                                                                                
NOTENTER MVC   RERROR,=AL2(586)    STATION IS NOT A ENTERPRISE STATION.         
         B     ERREND                                                           
                                                                                
NOTCOLM  MVC   RERROR,=AL2(587)    STATION IS NOT A COLUMBINE STATION.          
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
ACTCHGD  MVC   RERROR,=AL2(106)    RECORDS CHANGED.  PRESS ENTER TO             
         LA    R2,TKOSRCEH           PROCEED                                    
         B     RINFEND                                                          
                                                                                
GOADD    MVC   RERROR,=AL2(107)    PLEASE ADD RECORDS.                          
         LA    R2,TKOACTH                                                       
         B     RINFEND                                                          
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
INFEND   DS    0H                                                               
         LA    R2,TKOSRCEH         PUT CURSOR HERE                              
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
*                                                                               
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB5D          (OUR MAINTENANCE SCREEN OVERLAY)             
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
EOPREC   DSECT                                                                  
       ++INCLUDE REGENEOP                                                       
TKOREC   DSECT                                                                  
       ++INCLUDE REGENTKO                                                       
         DSECT                                                                  
       ++INCLUDE REGENADV                                                       
         DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
         DSECT                                                                  
       ++INCLUDE REGENOFF                                                       
         DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
         DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         DSECT                                                                  
       ++INCLUDE REGENCTY                                                       
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
TKOKEY   DS    CL(L'KEY)                                                        
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
**PAN#1  DC    CL21'093RESFM45S  12/14/99'                                      
         END                                                                    
