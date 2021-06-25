*          DATA SET RESFM43    AT LEVEL 158 AS OF 12/28/99                      
*PHASE T81843A                                                                  
         TITLE 'T81843 - RESFM43 - TKO EQUIVALENCY ADD'                         
***********************************************************************         
*                                                                     *         
*  RESFM43 (T81843) --- TKO MAINTENANCE/ADD                           *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 02DEC99 (BU ) ADD FEATURE VERSION:  NEW SCREEN (RESFM9A), DOUBLE    *         
*               LINE, DETAILING OLD/NEW EXPANSION, RESULT OF UPDATE   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T81843   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81843*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH                                                  
         DROP  RE                                                               
*                                  SET A(SWITC)                                 
         ST    R3,RELO                                                          
                                                                                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   IOOPT,C'Y'          DO MY OWN I/O'S                              
         OI    GENSTAT1,RDUPAPPL   ALLOW READ FOR UPDATE WITH                   
*                                   ACTION LIST                                 
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VALIKEY             ALL ACTIVITY FROM VALKEY                     
         B     EXIT                                                             
                                                                                
         MVI   SCRNFLAG,NEXTSCRN   WHAT WAS JUST ENTERED                        
         NI    TKASRCEH+4,X'FF'-X'20'  JUST ENSURES DISPLAY FROM TOP            
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
*                                                                               
*                                                                               
*   RETRIEVE ORIGINAL SYSTEM NUMBER, TO SUPPRESS SYSTEM SWITCH                  
*        IF SOURCE AND TARGET REPS ARE ON SAME SYSTEM                           
*        DO THIS ONLY IN VKNM, SO IT ONLY GETS ACCOMPLISHED ONCE                
*                                                                               
         XC    NEWCODES,NEWCODES                                                
         XC    OLDCODES,OLDCODES                                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   ORIGUTL,FASYS                                                    
*                                  SAVE ORIGINAL SYSTEM NUMBER                  
         MVC   SAVELUID,FASYM      GET LUID                                     
         DROP  RF                                                               
         MVI   SCRNFLAG,SAMESCRN   WE'RE NOT SCROLLING                          
*                                                                               
         LA    R2,TKASRCEH         A(SOURCE REP)                                
         CLI   5(R2),0             SOURCE REP MUST BE ENTERED                   
         BE    MISSFLD                                                          
         GOTO1 VALIREP                                                          
         BNZ   INVLFLD             ERROR:  REP NOT FOUND                        
         CLC   =C'B3',AGENCY       EJOR SIGNON?                                 
         BE    VKEY0040            YES - ALLOW ANY MOVEMENT                     
         CLC   =C'SJ',AGENCY       SJR  SIGNON?                                 
         BE    VKEY0040            YES - ALLOW ANY MOVEMENT                     
         CLC   =C'CCR',8(R2)       LIMIT THIS TO IR/CCRNY                       
         BNE   INVLFLD             ERROR: NOT ACCEPTABLE REP                    
         CLC   =C'NU',AGENCY       NOT TEST: ONLY ALLOW CCRSNY                  
         BNE   INVLFLD             ERROR                                        
VKEY0040 EQU   *                                                                
*                                                                               
*   REP HAS BEEN FOUND.  BOTH SOURCE AND ORIGINAL UTLS HAVE                     
*        BEEN SET, TO ENABLE SWITCHING BETWEEN FILES.                           
*                                                                               
         XC    TKOKEY,TKOKEY                                                    
         XC    FIRSTKEY,FIRSTKEY                                                
         MVI   FILTER,0                                                         
                                                                                
         LA    R2,TKAKADVH         POINT HERE IF ERROR                          
                                                                                
         CLI   TKAKADVH+5,0        ONE AND ONLY ONE OF THESE FIELDS             
         BE    VKEY0120            MUST HAVE AN INPUT                           
                                                                                
         MVC   ADDFLAG,TKAKADV     USE FIELD VALUE AS FLAG                      
         CLI   TKAKADV,C'+'        RESET + VALUE TO U                           
         BNE   VKEY0080                                                         
         MVI   TKAKADV,C'U'                                                     
VKEY0080 EQU   *                                                                
         FOUT  TKAKADVH                                                         
                                                                                
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
*                             1.3.5.7.9.1.3.5.7.9.1                             
         MVC   TKAIFLD(08),=C'ADVERT**'                                         
         MVC   TKAHFLD(08),=C'ADVERT**'                                         
                                                                                
VKEY0120 DS    0H                                                               
         CLI   TKAKAGYH+5,0                                                     
         BE    VKEY0140                                                         
                                                                                
         MVC   ADDFLAG,TKAKAGY     USE FIELD VALUE AS FLAG                      
         CLI   TKAKAGY,C'+'                                                     
         BNE   VKEY0130                                                         
         MVI   TKAKAGY,C'U'                                                     
VKEY0130 EQU   *                                                                
         FOUT  TKAKAGYH                                                         
                                                                                
         LA    R2,TKAKAGYH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
*                             1.3.5.7.9.1.3.5.7.9.1                             
         MVC   TKAIFLD(08),=C'AGY/OFFC'                                         
         MVC   TKAHFLD(08),=C'AGY/OFFC'                                         
                                                                                
VKEY0140 DS    0H                                                               
         CLI   TKAKSALH+5,0                                                     
         BE    VKEY0160                                                         
                                                                                
         MVC   ADDFLAG,TKAKSAL     USE FIELD VALUE AS FLAG                      
         CLI   TKAKSAL,C'+'                                                     
         BNE   VKEY0150                                                         
         MVI   TKAKSAL,C'U'                                                     
VKEY0150 EQU   *                                                                
         FOUT  TKAKSALH                                                         
                                                                                
         LA    R2,TKAKSALH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
*                             1.3.5.7.9.1.3.5.7.9.1                             
         MVC   TKAIFLD(08),=C'SALESPSN'                                         
         MVC   TKAHFLD(08),=C'SALESPSN'                                         
                                                                                
VKEY0160 DS    0H                                                               
         CLI   TKAKTYPH+5,0                                                     
         BE    VKEY0180                                                         
                                                                                
         MVC   ADDFLAG,TKAKTYP     USE FIELD VALUE AS FLAG                      
         CLI   TKAKTYP,C'+'                                                     
         BNE   VKEY0170                                                         
         MVI   TKAKTYP,C'U'                                                     
VKEY0170 EQU   *                                                                
         FOUT  TKAKTYPH                                                         
                                                                                
         LA    R2,TKAKTYPH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
*                             1.3.5.7.9.1.3.5.7.9.1                             
         MVC   TKAIFLD(08),=C'CON TYPE'                                         
         MVC   TKAHFLD(08),=C'CON TYPE'                                         
                                                                                
VKEY0180 EQU   *                                                                
         CLI   FILTER,1            SINGLE FILTER ENTERED?                       
         BE    VKEY0190            YES                                          
         MVI   ADDFLAG,X'00'       NO  - CLEAR UPDATE FLAG                      
*                                     THEN SEPARATE ERROR TYPES                 
         CLI   FILTER,0            MUST HAVE AT LEAST ONE FIELD                 
         BE    MISSFLD                                                          
         CLI   FILTER,1            CAN ONLY FILTER ON ONE FIELD                 
         BH    INVLFLD                                                          
VKEY0190 EQU   *                                                                
         FOUT  TKAIFLDH                                                         
         FOUT  TKAHFLDH                                                         
         CLI   ADDFLAG,C'+'        CLEAR SCREEN REQUEST?                        
         BNE   VKEY0000            NO  - UPDATE REQUEST                         
         GOTO1 CLRSCRN             ADD REQUEST: CLEAR THE SCREEN                
         B     GOADD                                                            
*                                                                               
*   CODE MOVED OUT OF LINE                                                      
VKEY0000 EQU   *                                                                
*                                                                               
         GOTO1 VALNEWCD            VALIDATE NEW CODES ON SCREEN                 
*                                                                               
         GOTO1 VALOLDCD            VALIDATE OLD CODES ON SCREEN                 
         L     R2,DUB              SET A(RECORD TYPE IN PROGRESS)               
VKEY0015 EQU   *                                                                
         GOTO1 PROCACT             CYCLE TABLE TO RECORDS                       
*                                  ALL VALIDATION DONE AT THIS POINT            
         B     JOBDONE                                                          
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
         LA    RF,TKASRCEH         A(SOURCE REP)                                
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
         B     VREP0040            FINISHED                                     
VREP0030 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   VREP0040            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    VREP0060            YES                                          
VREP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VREP0020            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
VREP0060 EQU   *                                                                
         MVC   SRCEUTL,3(R1)       SAVE SOURCE UTL NUMBER                       
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
VREP0800 MVC   WORK+15(0),TKASRCE  LOAD SOURCE REP BY LENGTH                    
         EJECT                                                                  
*                                                                               
PROCACT  NTR1                                                                   
         LA    R2,TKAACTH          SET A(FIRST ACTION FIELD)                    
         LA    R3,NEWCODES         SET A(FIRST NEW CODE)                        
         LA    R4,OLDCODES         SET A(FIRST OLD CODE)                        
*                                                                               
PACT0020 EQU   *                                                                
         CLC   =C'ADD',8(R2)       ACTION ADD                                   
         BE    PROCADD                                                          
         CLI   8(R2),C'A'                                                       
         BE    PROCADD                                                          
                                                                                
**       CLC   =C'CHA',8(R2)       ACTION CHANGE                                
**       BE    PROCGET                                                          
**       CLI   8(R2),C'C'                                                       
**       BE    PROCGET                                                          
                                                                                
***      CLC   =C'DEL',8(R2)       ACTION DELETE                                
***      BE    PROCGET                                                          
                                                                                
***      CLC   =C'RES',8(R2)       ACTION RESTORE                               
***      BE    PROCGET                                                          
PACT0040 EQU   *                                                                
         LA    R3,6(R3)            BUMP NEW CODE TABLE                          
         LA    R4,6(R4)            BUMP OLD CODE TABLE                          
         LA    R2,TKALACTH-TKAACTH(R2)                                          
*                                  BUMP TO NEXT ACTION FIELD                    
         LA    RF,TKAFINH          TEST FOR END OF SCREEN                       
         CR    R2,RF               END OF SCREEN REACHED?                       
         BL    PACT0020            NO  - GO BACK FOR NEXT                       
         XIT1                      INVALID ACTION                               
         EJECT                                                                  
*                                                                               
PROCADD  DS    0H                  PROCESS ACTION ADD                           
         L     R6,AIO2             USE IO2                                      
         XC    0(256,R6),0(R6)                                                  
         USING RTKOREC,R6                                                       
         MVI   RTKOKTYP,X'1F'      INSERT RECORD TYPE                           
         MVC   RTKOKREP,AGENCY     INSERT TARGET REP CODE                       
         MVC   RTKOKORP,SRCEREP    INSERT SOURCE REP CODE                       
         MVC   RTKOKRTP,RECTYPE    INSERT RECORD TYPE                           
*                                                                               
*   THIS IS FIRST PART OF KEY, WILL NOT CHANGE AS TABLE IS PROCESSED            
*                                                                               
         LA    R0,7                SEVEN FIELDS ON SCREEN: LOOP                 
PADD0020 EQU   *                                                                
         OC    0(6,R3),0(R3)       ANY ENTRY IN NEW TABLE?                      
         BZ    PACT0040            NO  - SKIP THIS ENTRY                        
         OC    0(6,R4),0(R4)       ANY ENTRY IN OLD TABLE?                      
         BZ    PACT0040            NO  - SKIP THIS ENTRY                        
         MVC   RTKOKCOD,0(R4)      INSERT OLD CODE INTO KEY                     
***>>>>> OC    RTKOKCOD,SPACES     SPACE PAD                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING RTKOELEM,R5                                                      
         MVI   RTKOCODE,X'01'      SET ELEMENT CODE                             
         MVI   RTKOELLN,24         SET ELEMENT LENGTH                           
         MVC   RTKOEQIV,0(R3)      INSERT TARGET CODE VALUE                     
         CLI   RTKOKRTP,3          S/P RECORD?                                  
         BNE   PADD0030            NO                                           
         MVC   RTKOEQIV+3(3),SPACES                                             
*                                  CLEAR OUT OFFICE FIELD                       
         MVC   RTKOEQIV+3(2),3(R3) INSERT TARGET S/P OFF                        
         MVC   RTKOSPTM,5(R3)      INSERT TARGET S/P TEAM                       
PADD0030 EQU   *                                                                
         MVC   RTKOLUID,SAVELUID                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RTKODATE)                                   
*                                  INSERT TODAY'S DATE                          
         MVC   RTKODCHG,RTKODATE   SET DATE OF LAST CHANGE                      
PADD0040 DS    0H                                                               
         MVC   KEY,0(R6)                                                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 HIGH                                                             
         CLI   RECTYPE,1           AGENCY RECORD?                               
         BNE   PADD0042            NO                                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PADD0080            KEY NOT ON FILE                              
         B     PADD0058                                                         
PADD0042 EQU   *                                                                
         CLI   RECTYPE,2           ADVERT RECORD?                               
         BNE   PADD0044            NO                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PADD0080            KEY NOT ON FILE                              
         B     PADD0058                                                         
PADD0044 EQU   *                                                                
         CLI   RECTYPE,3           S/P    RECORD?                               
         BNE   PADD0046            NO                                           
         CLC   KEY(24),KEYSAVE                                                  
         BNE   PADD0080            KEY NOT ON FILE                              
         B     PADD0058                                                         
PADD0046 EQU   *                                                                
         CLI   RECTYPE,4           CONTYP RECORD?                               
         BNE   PADD0048            NO                                           
         CLC   KEY(22),KEYSAVE                                                  
         BNE   PADD0080            KEY NOT ON FILE                              
         B     PADD0058                                                         
PADD0048 EQU   *                                                                
         DC    H'0'                UNKNOWN RECORD TYPE                          
PADD0058 EQU   *                                                                
         TM    KEY+27,X'80'        ON FILE:  DELETED?                           
         BNO   PADD0060            NOT DELETED                                  
         LR    RF,R2                                                            
         LA    RF,TKAFINLH-TKAACTH(RF)                                          
         MVC   8(20,RF),=C'ON FILE AS DELETED  '                                
         FOUT  (RF)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     PACT0040            GO BACK FOR NEXT FIELD                       
PADD0060 EQU   *                                                                
         LR    RF,R2                                                            
         LA    RF,TKAFINLH-TKAACTH(RF)                                          
         MVC   8(20,RF),=C'ON FILE AS ACTIVE   '                                
         FOUT  (RF)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     PACT0040            RECORD ALREADY ON FILE                       
                                                                                
PADD0080 DS    0H                                                               
         MVC   AIO,AIO2                                                         
                                                                                
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         TM    DMCB+8,X'20'        RECORD ALREADY EXISTS?                       
         BNO   PADD0100            NO  - GO BACK FOR NEXT                       
         LR    RF,R2                                                            
         LA    RF,TKAFINLH-TKAACTH(RF)                                          
         MVC   8(20,RF),=C'ON FILE AS ACTIVE   '                                
         FOUT  (RF)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     PACT0040            RECORD ALREADY ON FILE                       
PADD0100 EQU   *                                                                
         LR    RF,R2                                                            
         LA    RF,TKAFINLH-TKAACTH(RF)                                          
         MVC   8(20,RF),=C'TKO ADDED TO FILE   '                                
         FOUT  (RF)                                                             
         MVC   8(3,R2),=C'***'                                                  
         FOUT  (R2)                                                             
         B     PACT0040                                                         
         DROP  R5,R6                                                            
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
         CLI   RTKOKRTP,1          AGENCY RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,4                SET L(CODE FIELD) FOR ADVERT                 
         CLI   RTKOKRTP,2          ADVERT RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,3                SET L(CODE FIELD) FOR S/P                    
         CLI   RTKOKRTP,3          S/P    RECORD?                               
         BE    PRGE0020                                                         
         LA    RF,1                SET L(CODE FIELD) FOR CONTYPE                
PRGE0020 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,PRGE0040         MOVE CODE BY LENGTH                          
         EX    RF,PRGE0050         SPACE FILL SIGNIFICANT CODE LENGTH           
         B     PRGE0060                                                         
PRGE0040 MVC   RTKOKCOD(0),8(R3)                                                
PRGE0050 OC    RTKOKCOD(0),SPACES                                               
*                                  MOVE N CHARS FROM SCREEN FIELD               
         DROP  R6                                                               
PRGE0060 EQU   *                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
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
         EJECT                                                                  
PROCCHA  DS    0H                                                               
         L     R6,AIO                                                           
         USING RTKOREC,R6                                                       
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RTKODCHG)                                   
         LR    R3,R2                                                            
                                                                                
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
         LA    R4,TKASOFFH-TKASTRTH(R4)                                         
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
         LA    RF,NEWCODES         SET A(NEXT SLOT IN TABLE)                    
         ST    RF,ACODES           SET A(NEXT SLOT)                             
*                                                                               
         XC    BYTE,BYTE           CLEAR FLAG                                   
         LA    R2,TKAACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VNEW0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VNEW0200                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VNEW0200            YES, THEN SKIP IT                            
         XC    KEY,KEY             CLEAR THE KEY                                
         CLC   =C'ADV',TKAHFLD     ADVERTISER?                                  
         BNE   VNEW0040            NO                                           
         MVI   RECTYPE,2           SET OUTPUT REC TO ADV                        
         LA    RF,TKAKADVH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),4             EXCEED MAX LENGTH?                           
         BH    VNEW0520            YES                                          
         MVC   KEY+21(4),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+21(4),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXPH-TKASTRTH(R4)                                          
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RADVNAME                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(4,RF),KEY+21      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         B     VNEW0220            YES - CHECK NEXT FIELD                       
         DROP  R6                                                               
VNEW0040 EQU   *                                                                
         CLC   =C'AGY',TKAHFLD     AGENCY?                                      
         BNE   VNEW0140            NO                                           
         MVI   RECTYPE,1           SET OUTPUT REC TO AGY                        
         LA    RF,TKAKAGYH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'0A'           SET 'AGENCY KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R3),(1,WORK),C',=,-'                               
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,WORK             SET L(AGY CODE PORTION)                      
         CH    RF,=H'4'                                                         
         BNH   VNEW0060            4 CHARS OR LESS                              
         MVC   KEY+19(6),WORK+12   MORE THAN 4: 6-CHAR CODE IN KEY              
         B     VNEW0080            IGNORE POSSIBLE OFFICE                       
VNEW0060 EQU   *                                                                
         MVC   KEY+19(4),WORK+12   <= 4: INSERT AGY CODE INTO KEY               
         MVC   KEY+23(2),WORK+22   INSERT AGY/OFF (MAY BE EMPTY)                
VNEW0080 EQU   *                                                                
         OC    KEY+19(6),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),AGENCY    INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXPH-TKASTRTH(R4)                                          
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RAGYNAM1                                                
         FOUT  (R4)                                                             
         CLC   KEY+23(2),=C'  '    AGENCY OFFICE ENTERED?                       
         BH    VNEW0120            YES                                          
         MVC   AGYKEY,KEY          SAVE KEY                                     
VNEW0100 EQU   *                                                                
         GOTO1 SEQ                 CHECK FOR OFFICE REQUIREMENT                 
         CLC   KEY(23),KEYSAVE     SAME AGENCY FOUND?                           
         BNE   VNEW0110            NO  - NO OFFICE NEEDED                       
         CLC   KEY+25(2),AGENCY    YES - SAME REP?                              
         BE    VNEW0540            YES - ERROR: NEEDS OFFICE                    
         B     VNEW0100            GO BACK FOR NEXT                             
VNEW0110 EQU   *                                                                
         MVC   KEY(27),AGYKEY      RESET KEY                                    
VNEW0120 EQU   *                                                                
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(6,RF),KEY+19      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         B     VNEW0220            YES - CHECK NEXT FIELD                       
         DROP  R6                                                               
VNEW0140 EQU   *                                                                
         CLC   =C'SAL',TKAHFLD     SALESPERSON?                                 
         BNE   VNEW0160            NO                                           
         MVI   RECTYPE,3           SET OUTPUT REC TO S/P                        
         LA    RF,TKAKSALH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'06'           SET 'S/P    KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),3             EXCEED MAX LENGTH?                           
         BH    VNEW0520            YES                                          
         MVC   KEY+22(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+24(3),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+24(3),SPACES    SET BIN ZERO TO SPACE                        
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXPH-TKASTRTH(R4)                                          
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RSALNAME                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(3,RF),KEY+24      INSERT CODE INTO TABLE                       
         MVC   3(2,RF),RSALOFF     INSERT S/P OFFICE INTO TABLE                 
         MVC   5(2,RF),RSALTEAM    INSERT S/P TEAM INTO TABLE                   
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         B     VNEW0220            CHECK NEXT FIELD                             
         DROP  R6                                                               
VNEW0160 EQU   *                                                                
         CLC   =C'CON',TKAHFLD     CONTRACT TYPE?                               
         BE    VNEW0180            NO                                           
         DC    H'0'                SHOULD NEVER HAPPEN                          
VNEW0180 EQU   *                                                                
         MVI   RECTYPE,4           SET OUTPUT REC TO CONTYPE                    
         LA    RF,TKAKTYPH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'32'           SET 'CONTYP KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF               BUMP TO NEXT FIELD                           
         CLI   5(R3),1             EXCEED MAX LENGTH?                           
         BH    VNEW0520            YES                                          
         MVC   KEY+24(2),AGENCY    INSERT REP CODE INTO KEY                     
         MVC   KEY+26(1),8(R3)     INSERT CODE IN KEY                           
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VNEW0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RCTYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXPH-TKASTRTH(R4)                                          
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RCTYDESC                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(1,RF),KEY+26      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         DROP  R6                                                               
         B     VNEW0220            YES - CHECK NEXT FIELD                       
VNEW0200 DS    0H                                                               
         L     RF,ACODES           SKIP THIS SLOT IN TABLE                      
         LA    RF,6(RF)                                                         
         ST    RF,ACODES                                                        
VNEW0220 DS    0H                                                               
         LA    RF,TKALACTH-TKAACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,TKAFINH                                                       
         CR    R2,R1                                                            
         BL    VNEW0020            GO BACK FOR NEXT                             
         B     VNEW0920            FINISHED                                     
VNEW0500 DS    0H                  TARGET REP CODE NOT FOUND                    
         MVI   BYTE,1              SET ERROR FLAG                               
         LR    R6,R3               SET A(TARGET CODE FIELD)                     
         LA    R6,TKAFINLH-TKASTRTH(R6)                                         
*                                  POINT TO 'RESULT' FIELD                      
         MVC   8(20,R6),=C'TRGT CODE NOT FOUND '                                
         FOUT  (R6)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     VNEW0200            GO DO NEXT FIELD                             
VNEW0520 DS    0H                  TARGET REP WRONG LENGTH                      
         MVI   BYTE,1              SET ERROR FLAG                               
         LR    R6,R3               SET A(TARGET CODE FIELD)                     
         LA    R6,TKAFINLH-TKASTRTH(R6)                                         
*                                  POINT TO 'RESULT' FIELD                      
         MVC   8(20,R6),=C'TRGT CODE FORMAT NG '                                
         FOUT  (R6)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     VNEW0200            GO DO NEXT FIELD                             
VNEW0540 DS    0H                  TARGET REP WRONG LENGTH                      
         MVI   BYTE,1              SET ERROR FLAG                               
         LR    R6,R3               SET A(TARGET CODE FIELD)                     
         LA    R6,TKAFINLH-TKASTRTH(R6)                                         
*                                  POINT TO 'RESULT' FIELD                      
         MVC   8(20,R6),=C'AGENCY NEEDS OFFICE '                                
         FOUT  (R6)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     VNEW0200            GO DO NEXT FIELD                             
VNEW0920 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         CLI   BYTE,0              ERROR ENCOUNTERED?                           
         BE    VNEW0990            NO                                           
         LR    R0,RB               LOAD NON-ZERO FOR CC                         
         LTR   R0,R0               SET CC NOT ZERO                              
VNEW0990 EQU   *                                                                
         ST    R0,DUB+4            SAVE CC SETTER                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   VALOLDCD:  SCAN SCREEN.  IF CODE ENTERED, VALIDATE FOR EXISTENCE.           
*                                                                               
VALOLDCD NTR1                                                                   
         GOTO1 SWSRCREP            SWITCH TO SOURCE REP                         
         LA    RF,OLDCODES         SET A(NEXT SLOT IN TABLE)                    
         ST    RF,ACODES           SET A(NEXT SLOT)                             
*                                                                               
         XC    BYTE,BYTE           CLEAR FLAG                                   
         LA    R2,TKAACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VOLD0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VOLD0420                                                         
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VOLD0420            YES, THEN SKIP IT                            
         XC    KEY,KEY             CLEAR THE KEY                                
         CLC   =C'ADV',TKAHFLD     ADVERTISER?                                  
         BNE   VOLD0040            NO                                           
         LA    RF,TKAKADVH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,8               SET 'ADVERTISER KEY'                         
         LR    R3,R2               BUMP TO CODE FIELD                           
         LA    R3,TKACODEH-TKAACTH(R3)                                          
*                                  BUMP TO SRCE CODE FIELD                      
         CLI   5(R3),4             EXCEED MAX LENGTH?                           
         BH    VOLD0520            YES                                          
         MVC   KEY+21(4),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+21(4),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),SRCEREP   INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VOLD0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXP2H-TKACODEH(R4)                                         
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RADVNAME                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(4,RF),KEY+21      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         DROP  R6                                                               
         B     VOLD0440            YES - CHECK NEXT FIELD                       
VOLD0040 EQU   *                                                                
         CLC   =C'AGY',TKAHFLD     AGENCY?                                      
         BNE   VOLD0100            NO                                           
         LA    RF,TKAKAGYH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'0A'           SET 'AGENCY KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         LA    R3,TKACODEH-TKAACTH(R3)                                          
*                                  BUMP TO SRCE CODE FIELD                      
         GOTO1 SCANNER,DMCB,(R3),(1,WORK),C',=,-'                               
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,WORK             SET L(AGY CODE PORTION)                      
         CH    RF,=H'4'                                                         
         BNH   VOLD0060            4 CHARS OR LESS                              
         MVC   KEY+19(6),WORK+12   MORE THAN 4: 6-CHAR CODE IN KEY              
         B     VOLD0080            IGNORE POSSIBLE OFFICE                       
VOLD0060 EQU   *                                                                
         MVC   KEY+19(4),WORK+12   <= 4: INSERT AGY CODE INTO KEY               
         MVC   KEY+23(2),WORK+22   INSERT AGY/OFF (MAY BE EMPTY)                
VOLD0080 EQU   *                                                                
         OC    KEY+19(6),SPACES    SET BIN ZERO TO SPACE                        
         MVC   KEY+25(2),SRCEREP   INSERT REP CODE INTO KEY                     
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VOLD0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXP2H-TKACODEH(R4)                                         
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RAGYNAM1                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(6,RF),KEY+19      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         DROP  R6                                                               
         B     VOLD0440            YES - CHECK NEXT FIELD                       
VOLD0100 EQU   *                                                                
         CLC   =C'SAL',TKAHFLD     SALESPERSON?                                 
         BNE   VOLD0120            NO                                           
         LA    RF,TKAKSALH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'06'           SET 'S/P    KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         LA    R3,TKACODEH-TKAACTH(R3)                                          
*                                  BUMP TO SRCE CODE FIELD                      
         CLI   5(R3),3             EXCEED MAX LENGTH?                           
         BH    VOLD0520            YES                                          
         MVC   KEY+22(2),SRCEREP   INSERT REP CODE INTO KEY                     
         MVC   KEY+24(3),8(R3)     INSERT CODE IN KEY                           
         OC    KEY+24(3),SPACES    SET BIN ZERO TO SPACE                        
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VOLD0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXP2H-TKACODEH(R4)                                         
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RSALNAME                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(3,RF),KEY+24      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         B     VOLD0440            CHECK NEXT FIELD                             
VOLD0120 EQU   *                                                                
         CLC   =C'CON',TKAHFLD     CONTRACT TYPE?                               
         BE    VOLD0140            NO                                           
         DC    H'0'                SHOULD NEVER HAPPEN                          
VOLD0140 EQU   *                                                                
         LA    RF,TKAKTYPH                                                      
         ST    RF,DUB              SAVE CURSOR FLD ON ERROR                     
         MVI   KEY,X'32'           SET 'CONTYP KEY'                             
         LR    R3,R2               BUMP TO CODE FIELD                           
         LA    R3,TKACODEH-TKAACTH(R3)                                          
*                                  BUMP TO SRCE CODE FIELD                      
         CLI   5(R3),1             EXCEED MAX LENGTH?                           
         BH    VOLD0520            YES                                          
         MVC   KEY+24(2),SRCEREP   INSERT REP CODE INTO KEY                     
         MVC   KEY+26(1),8(R3)     INSERT CODE IN KEY                           
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VOLD0500            NO  - ERROR: CODE NOT FOUND                  
         L     R6,AIO                                                           
         USING RCTYREC,R6                                                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         LR    R4,R3                                                            
         LA    R4,TKAEXP2H-TKACODEH(R4)                                         
*                                  BUMP TO EXPANSION FIELD                      
         MVC   8(20,R4),RCTYDESC                                                
         FOUT  (R4)                                                             
         L     RF,ACODES           INSERT CODE IN NEXT SLOT                     
         MVC   0(1,RF),KEY+26      INSERT CODE INTO TABLE                       
         LA    RF,6(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ACODES                                                        
         DROP  R6                                                               
         B     VOLD0440            YES - CHECK NEXT FIELD                       
VOLD0420 DS    0H                                                               
         L     RF,ACODES           SKIP THIS SLOT IN TABLE                      
         LA    RF,6(RF)                                                         
         ST    RF,ACODES                                                        
VOLD0440 DS    0H                                                               
         LA    RF,TKALACTH-TKAACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,TKAFINH                                                       
         CR    R2,R1                                                            
         BL    VOLD0020            GO BACK FOR NEXT                             
         B     VOLD0920            FINISHED                                     
VOLD0500 DS    0H                  TARGET REP CODE NOT FOUND                    
         MVI   BYTE,1              SET ERROR FLAG                               
         LR    R6,R3               SET A(TARGET CODE FIELD)                     
         LA    R6,TKAFINLH-TKACODEH(R6)                                         
*                                  POINT TO 'RESULT' FIELD                      
         MVC   8(20,R6),=C'SRCE CODE NOT FOUND '                                
         FOUT  (R6)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     VOLD0420            GO DO NEXT FIELD                             
VOLD0520 DS    0H                  TARGET REP WRONG LENGTH                      
         MVI   BYTE,1              SET ERROR FLAG                               
         LR    R6,R3               SET A(TARGET CODE FIELD)                     
         LA    R6,TKAFINLH-TKACODEH(R6)                                         
*                                  POINT TO 'RESULT' FIELD                      
         MVC   8(20,R6),=C'SRCE CODE FORMAT NG '                                
         FOUT  (R6)                                                             
         MVC   8(3,R2),=C'*ER'                                                  
         FOUT  (R2)                                                             
         B     VOLD0420            GO DO NEXT FIELD                             
VOLD0920 EQU   *                                                                
         GOTO1 SWTGTREP            SWITCH BACK TO TARGET REP                    
         SR    R0,R0               SET CC = ZERO                                
         CLI   BYTE,0              ERROR ENCOUNTERED?                           
         BE    VOLD0990            NO                                           
         LR    R0,RB               SET CC NOT ZERO                              
         LTR   RB,RB               SET CC NOT ZERO                              
VOLD0990 EQU   *                                                                
         ST    R0,DUB+4            SAVE CC SETTER                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*  SWITCH TO SOURCE REP FILE                                         *          
**********************************************************************          
SWSRCREP NTR1                                                                   
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWSR0020            YES - DON'T SWITCH                           
         GOTO1 VSWITCH,DMCB,(SRCEUTL,X'FFFFFFFF'),0                             
                                                                                
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    SWSR0020            YES - NOW READ CONTRACT RECORDS              
         CLI   4(R1),2             NO  - SYSTEM NOT OPENED?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO  - OTHER REASON                           
*                                                                               
         B     SWTGTSET                                                         
*                                                                               
*                                                                               
SWSR0020 DS    0H                                                               
         XIT1                      SWITCHED TO SOURCE REP                       
*                                                                               
SWTGTSET NTR1                      ERROR: SWITCH BACK TO TARGET REP             
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SRCCLOSD            YES - EXIT WITH MESSAGE                      
         DC    H'0'                NO  - ABORT                                  
*                                                                               
SWTGTREP NTR1                      SWITCH BACK TO TARGET REP                    
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWTG0020            YES - DON'T SWITCH                           
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SWTG0020            YES - EXIT ROUTINE                           
         DC    H'0'                NO  - ABORT                                  
SWTG0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLEARCRN:  REMOVE ALL ENTRIES FROM SCREEN TO PERMIT NEW ADDS                
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,TKAACTH          CLEAR ALL FIELDS ON SCREEN                   
CLSC0020 EQU   *                                                                
         LR    RF,R2                                                            
         LR    R3,R2                                                            
         LA    RF,TKALACTH-TKAACTH(RF)                                          
*                                  SET END OF CURRENT LINE                      
CLSC0040 EQU   *                                                                
         ZIC   RE,0(R3)            FIELD LENGTH W/CONTROL HDR                   
         SH    RE,=H'9'            SUBTRACT CONTROL HDR + 1 FOR EX              
         EX    RE,CLSC0800         CLEAR FIELD BY LENGTH                        
         FOUT  (R3)                                                             
         ZIC   RE,0(R3)            BUMP TO NEXT FIELD                           
         AR    R3,RE                                                            
         CR    R3,RF               END OF LINE REACHED?                         
         BL    CLSC0040            NO  - GO BACK FOR NEXT FIELD                 
         LA    R2,TKALACTH-TKAACTH(R2)                                          
*                                  BUMP TO NEXT LINE                            
         LA    RE,TKAFINH          CHECK FOR END OF SCREEN                      
         CR    R2,RE                                                            
         BL    CLSC0020            GO BACK FOR NEXT LINE                        
         B     CLSC0900                                                         
CLSC0800 EQU   *                                                                
         XC    8(0,R3),8(R3)       CLEAR BY LENGTH                              
CLSC0900 EQU   *                                                                
         XIT1                      DONE - EXIT                                  
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
*                                                                               
SRCCLOSD MVC   RERROR,=AL2(658)                                                 
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
         LA    R2,TKASRCEH           PROCEED                                    
         B     RINFEND                                                          
                                                                                
GOADD    MVC   RERROR,=AL2(107)    PLEASE ADD RECORDS.                          
         LA    R2,TKAACTH                                                       
         B     RINFEND                                                          
                                                                                
JOBDONE  MVC   RERROR,=AL2(173)    UPDATE COMPLETE.  SEE 'RESULTS'              
         LA    R2,TKAACTH                                                       
         B     RINFEND                                                          
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
INFEND   DS    0H                                                               
         LA    R2,TKASRCEH         PUT CURSOR HERE                              
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
       ++INCLUDE RESFM9AD          (OUR MAINTENANCE SCREEN OVERLAY)             
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
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
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
SRCEUTL  DS    X                                                                
ORIGUTL  DS    X                                                                
RECTYPE  DS    X                                                                
CLCLEN   DS    X                                                                
SAVELUID DS    CL8                                                              
NEWCODES DS    CL48                8 NEW CODES FROM SCREEN MAX                  
OLDCODES DS    CL48                8 OLD CODES FROM SCREEN MAX                  
ACODES   DS    A                   NEXT SLOT IN TABLE                           
VSWITCH  DS    F                                                                
AGYKEY   DS    CL27                SAVE AREA FOR AGENCY KEY                     
*                                                                               
       ++INCLUDE FLDIND                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158RESFM43   12/28/99'                                      
         END                                                                    
