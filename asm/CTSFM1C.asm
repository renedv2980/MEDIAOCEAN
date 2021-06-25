*          DATA SET CTSFM1C    AT LEVEL 118 AS OF 05/01/02                      
*PHASE TA0A1CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A1C - ADDS STATION RECORD MAINTENANCE/LIST               *         
*                                                                     *         
*  COMMENTS: MAINTAINS ADDS STATION RECORDS.                          *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMC6 (TA0AC6) -- MAINTENANCE                    *         
*                  CTSFMC7 (TA0AC7) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED ADDS STATION RECORDS                              *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A1C ADDS STATION RECORD MAINTENANCE/LIST'                    
TA0A1C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A1C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'Y'       CREATE ACTIVITY ELEMENT ON RECORDS           
*                                                                               
         OI    GLSTSTAT,RETEXTRA   LET LISTMON RETURN IF END OF SCREEN          
*                                                                               
         BAS   RE,SETREP           SETUP REP TABLE POINTERS                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED                  
         BE    XA                                                               
         CLI   MODE,XRECPUT        AFTER RECORD HAS BEEN PUT                    
         BE    XP                                                               
         CLI   MODE,XRECDEL        AFTER RECORD HAS BEEN DELETED                
         BE    XD                                                               
         CLI   MODE,XRECREST       AFTER RECORD HAS BEEN RESTORED               
         BE    XR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* VK -   VALIDATE KEY ROUTINE                                                   
* ********************************************************************          
*                                                                               
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING STTNKEY,R3                                                       
         MVI   STTNSYS,STTNSYSQ    KEY SYSTEM X'05' FOR ALL SYSTEMS             
         MVI   STTNTYP,STTNTYPQ    TYPE X'06' FOR ADDS STATION REC              
*                                                                               
VK10     DS    0H                                                               
*                                                                               
* IF JB, ONLY MEDIA RADIO IS VALID                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         CLC   =C'JB',FALINE                                                    
         BNE   VK15                                                             
         LA    R2,STAMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'R'                                                       
         BNE   INVLMED                                                          
         DROP  RF                                                               
*                                                                               
VK15     CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   VK18                                                             
*                                                                               
         MVI   BITFLAG,0           ASSUME KEY/FILTERS NOT CHANGED               
*                                                                               
         LA    R2,LSTMEDH          THEN MEDIA MUST EXIST                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'80'         MEDIA CHANGED THIS TIME?                     
         BZ    *+8                                                              
         OI    BITFLAG,X'80'       YES                                          
*                                                                               
         LA    R2,LSTIDH                                                        
         TM    4(R2),X'80'         STATION ID CHANGED THIS TIME?                
         BZ    *+8                                                              
         OI    BITFLAG,X'40'       YES                                          
*                                                                               
         LA    R2,LSTREPH                                                       
         TM    4(R2),X'80'         REP FILTER CHANGED THIS TIME?                
         BZ    *+8                                                              
         OI    BITFLAG,X'20'       YES                                          
*                                                                               
         LA    R2,LSTDATH                                                       
         TM    4(R2),X'80'         DATE FILTER CHANGED THIS TIME?               
         BZ    *+8                                                              
         OI    BITFLAG,X'10'       YES                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK16                                                             
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),MYWORK                                 
         TM    DMCB+4,X'01'                                                     
         BO    INVLFLD                                                          
         LA    R1,MYWORK                                                        
         MVC   THISDATE,PVALBSTA-PERVALD(R1)                                    
*                                                                               
VK16     LA    R2,LSTHOMEH                                                      
         TM    4(R2),X'80'         HOME MARKET CHANGED THIS TIME?               
         BZ    *+8                                                              
         OI    BITFLAG,X'08'       YES                                          
*                                                                               
         B     VKX                 THAT'S IT FOR LIST AND REPORT                
*                                                                               
VK18     LA    R2,STAIDH                                                        
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),0                                                          
         BE    MISSFLD                                                          
         CLI   0(R4),3                                                          
         BL    INVLSTA                                                          
         CLI   0(R4),4                                                          
         BH    INVLSTA                                                          
         TM    2(R4),X'40'         TEST ALPHABETIC                              
         BZ    INVLSTA                                                          
         MVC   STTNCALL(4),12(R4)  SAVE CALL LETTERS                            
*                                                                               
         CLI   STAMED,C'R'         TEST MEDIA IS RADIO                          
         BE    VK30                                                             
*                                                                               
* MEDIA NOT RADIO *                                                             
*                                                                               
         CLI   1(R4),0             TEST SUB-MEDIA ENTERED                       
         BNE   VK20                YES                                          
         MVC   STTNCALL+4(1),STAMED  ELSE SET SUB-MED = MEDIA                   
         B     VK40                                                             
VK20     MVC   STTNCALL+4(1),22(R4) MOVE SUB-MEDIA                              
         CLI   1(R4),1                                                          
         BNE   INVLSTA                                                          
         CLC   STTNCALL+4(1),STAMED     IF INPUT, MUST MATCH MEDIA CODE         
         BNE   INVLSTA                                                          
         B     VK40                                                             
*                                                                               
* MEDIA = RADIO - AM OR FM IS REQUIRED *                                        
*                                                                               
VK30     DS    0H                                                               
         CLI   1(R4),2                                                          
         BH    INVLSTA                                                          
         MVC   STTNCALL+4(1),22(R4)                                             
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,VKAM                                                          
         BE    VK40                                                             
         EX    R5,VKFM                                                          
         BE    VK40                                                             
         B     INVLSTA                                                          
*                                                                               
VKAM     CLC   22(0,R4),=C'AM'                                                  
VKFM     CLC   22(0,R4),=C'FM'                                                  
*                                                                               
VK40     B     VK55                BYPASS VALIDATION-- AS PER CHRIS O'          
*                                                                               
         L     RF,ACOMFACS         VALIDATE STATION CALL LETTERS                
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHS             USER NOT AUTHORISED FOR SPOT SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STTNCALL                                                
*                                                                               
         TM    DBERROR,X'10'                                                    
         BNO   VK42                                                             
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STTNCALL                                                
VK42     L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
*                                                                               
VK50     L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHC             USER NOT AUTHORISED FOR CTRL SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         TM    DBERROR,X'10'                                                    
         BO    INVLSTA                                                          
*                                                                               
VK55     MVC   STATION,STTNCALL                                                 
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* ********************************************************************          
* VR -   VALIDATE RECORD ROUTINE                                                
* ********************************************************************          
*                                                                               
VR       DS    0H                                                               
         XC    MYNBMKT,MYNBMKT                                                  
         XC    MYABMKT,MYABMKT                                                  
         XC    MYAMKT,MYAMKT                                                    
         XC    ALPHMKT,ALPHMKT                                                  
*                                                                               
         LA    R2,STANEWH          NEW STATION?--BYPASS VALIDTION?N             
         OI    STANEWH+6,X'80'                                                  
         CLI   5(R2),0             NO, VALIDATE                                 
         BE    VR01                                                             
         CLI   STANEW,C'Y'         YES, BYPASS                                  
         BE    VR02                                                             
         CLI   STANEW,C'N'         NOT A NEW STATION--VALIDATE                  
         BNE   INVLFLD                                                          
*                                                                               
VR01     XC    STANEW,STANEW       DO VALIDATIONS                               
         LA    R2,STAIDH                                                        
         OI    STAIDH+6,X'80'                                                   
         L     RF,ACOMFACS         VALIDATE STATION CALL LETTERS                
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHS             USER NOT AUTHORISED FOR SPOT SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STATION                                                 
*                                                                               
         TM    DBERROR,X'10'                                                    
         BNO   VR01A               ELSE STATION IS OKAY                         
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STATION                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
*                                                                               
VR01A    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHC             USER NOT AUTHORISED FOR CTRL SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         TM    DBERROR,X'10'                                                    
         BO    INVLSTA             ELSE STATION IS OKAY                         
*                                                                               
VR02     CLI   STAMED,C'R'         FOR RADIO:                                   
         BNE   VR03                                                             
         NI    STAMKTAH+6,X'DF'    UNPROTECT ALPHA MKT                          
         OI    STAAMCH+6,X'20'     PROTECT ARB/NSI FIELDS                       
         OI    STANMCH+6,X'20'                                                  
         XC    STAAMC,STAAMC                                                    
         XC    STANMC,STANMC                                                    
         OI    STAAMCH+6,X'80'     TRANSMIT                                     
         OI    STANMCH+6,X'80'     TRANSMIT                                     
         B     VR04                                                             
*                                                                               
VR03     DS    0H                  FOR TV:                                      
         OI    STAMKTAH+6,X'20'    PROTECT ALPHA MKT                            
         NI    STAAMCH+6,X'DF'     UNPROTECT ARB/NSI FIELDS                     
         NI    STANMCH+6,X'DF'                                                  
         XC    STAMKTA,STAMKTA                                                  
         OI    STAMKTAH+6,X'80'     TRANSMIT                                    
         B     VR04                                                             
*                                  VALIDATE REP CODE                            
VR04     LA    R2,STARCDEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         OC    STARCDE(3),SPACES   BLANK PADDED                                 
         L     R4,AREPTAB          POINT TO REP TABLE                           
VR05     DS    0H                                                               
         CLI   0(R4),0             END OF TABLE??                               
         BE    INVLFLD             CAN'T FIND PREFIX                            
         CLC   STARCDE(3),12(R4)   COMPARE PREFIX                               
         BE    VR07                                                             
VR06     ZIC   RF,0(R4)            GET LENGTH OF REP ENTRY                      
         LA    R4,0(RF,R4)         BUMP TO NEXT ROW                             
         B     VR05                                                             
*                                                                               
VR07     DS    0H                                                               
         XC    EFFDATE,EFFDATE                                                  
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
*                                                                               
         L     R6,AIO                                                           
         USING STTNDSCD,R6                                                      
         MVI   ELCODE,STTNDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   SVABMKT,STTNDARB    SAVE FOR PASSIVE RECS                        
         MVC   SVNBMKT,STTNDNSI                                                 
         MVC   SVMKTA,STTNMKTA                                                  
         CLC   STTNDREP,STARCDE    REP CODE CHANGED??                           
         BE    VR10                                                             
*                                                                               
         LA    R2,STAPREPH         YES, DISPLAY PREVIOUS REP                    
         MVC   STAPREP,STTNDREP                                                 
         FOUT  (R2)                                                             
         DROP  R6                                                               
*                                                                               
         LA    R2,STADATEH                                                      
         CLI   5(R2),0             REP CODE CHANGED, NEED EFF. DATE             
         BE    MISSFLD                                                          
         MVI   FLAG,C'Y'           MAKE SURE DATE IS >=TODAY'S DATE             
         B     VR20                                                             
*                                                                               
VR10     DS    0H                  VALIDATE EFFECTIVE DATE                      
         MVI   FLAG,C'N'           IGNORE DATE >=TODAY'S DATE CHECK             
         LA    R2,STADATEH                                                      
         CLI   5(R2),0                                                          
         BE    VR30                                                             
*                                                                               
VR20     DS    0H                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),MYWORK                                 
         TM    DMCB+4,X'01'                                                     
         BO    INVLFLD                                                          
*                                                                               
         LA    R5,MYWORK                                                        
         USING PERVALD,R5                                                       
         CLI   FLAG,C'N'                                                        
         BE    VR25                                                             
         GOTO1 DATCON,DMCB,(5,0),(3,THISDATE)   GET TODAY'S DATE                
         CLC   PVALBSTA,THISDATE                                                
         BL    INVLFLD             EFFECTIVE FROM AT LEAST TODAY                
VR25     MVC   EFFDATE,PVALBSTA                                                 
         DROP  R5                                                               
*                                                                               
VR30     DS    0H                  VALIDATE OUPUT CLASS                         
         LA    R2,STAOUTCH                                                      
         CLI   5(R2),0                                                          
         BE    VR40                                                             
*                                                                               
         TM    4(R2),X'04'         VALID ALPHABETIC??                           
         BZ    INVLFLD                                                          
*                                                                               
VR40     DS    0H                  VALIDATE FAX NUMBER                          
         LA    R2,STAFAXH                                                       
         CLI   5(R2),0                                                          
         BE    VR50                                                             
*                                                                               
VR50     DS    0H                  VALIDATE ARB MARKET CODE                     
         CLI   STAMED,C'R'         IF RADIO, BYPASS                             
         BE    VR70                                                             
         LA    R2,STAAMCH                                                       
         CLI   5(R2),0                                                          
         BE    VR60                                                             
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC??                              
         BZ    INVLFLD                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R1,DUB                                                           
         STH   R1,MYABMKT                                                       
*                                  VALIDATE THAT RECORD EXISTS                  
         CLI   STANEW,C'Y'         BYPASS VALIDATION?                           
         BE    VR60                YES                                          
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHS             USER NOT AUTHORISED FOR SPOT SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STATION                                                 
         MVC   DBSELRMK,MYABMKT                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         TM    DBERROR,X'10'                                                    
         BNO   VR55                                                             
         B     INVLFLD                                                          
VR55     DS    0H                                                               
         L     R5,AIO3                                                          
         USING DMKEY,R5                                                         
         LA    R1,DMFRSTEL                                                      
         CLI   4(R1),C'*'          UNKNOWN RATING SERVICE MARKET NUMBER         
         BE    INVLFLD                                                          
         DROP  R5                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHC             USER NOT AUTHORISED FOR CTRL SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
VR60     DS    0H                  VALIDATE NSI MARKET CODE                     
         LA    R2,STANMCH                                                       
         CLI   5(R2),0                                                          
         BE    VR70                                                             
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC??                              
         BZ    INVLFLD                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R1,DUB                                                           
         STH   R1,MYNBMKT                                                       
*                                  VALIDATE THAT RECORD EXISTS                  
         CLI   STANEW,C'Y'         BYPASS VALIDATION?                           
         BE    VR70                YES                                          
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHS             USER NOT AUTHORISED FOR SPOT SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING DBLOCK,R4                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBAREC,AIO3                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED(1),STAMED                                               
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,=X'5B0B'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELSTA,STATION                                                 
         MVC   DBSELRMK,MYNBMKT                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         TM    DBERROR,X'10'                                                    
         BNO   VR65                                                             
         B     INVLFLD                                                          
*                                                                               
VR65     L     R5,AIO3                                                          
         USING DMKEY,R5                                                         
         LA    R1,DMFRSTEL                                                      
         CLI   4(R1),C'*'          UNKNOWN RATING SERVICE MARKET NUMBER         
         BE    INVLFLD                                                          
         DROP  R5                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,1                                                         
         BE    NOAUTHC             USER NOT AUTHORISED FOR CTRL SYSTEM          
         CLI   DMCB+4,0                                                         
         BNE   BADSWTC             SYSTEM NOT OPEN                              
         DROP  R4                                                               
*                                                                               
VR70     DS    0H                                                               
         CLI   STAMED,C'T'         BYPASS IF TV RECORD                          
         BE    VR70A                                                            
         LA    R2,STAMKTAH         VALIDATE ALPHA MARKET                        
         GOTO1 ANY                 FIELD REQUIRED                               
         TM    4(R2),X'04'         VALID ALPHA?                                 
         BZ    INVLFLD                                                          
         MVC   ALPHMKT,WORK                                                     
         BAS   RE,VALMKT           VALID ALPHA MKT? (CHECK FILE)                
         BZ    INVLFLD                                                          
         MVC   MYAMKT,ALPHMKT                                                   
*                                                                               
VR70A    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR75                                                             
*                                                                               
         MVI   ELCODE,STTNDCDQ                                                  
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STTNHCDQ     REMOVE HOME MKT CODES ELEMENT                
         GOTO1 REMELEM                                                          
*                                                                               
VR75     DS    0H                  ADD DESCRIPTION ELEMENT                      
         LA    R6,ELEM                                                          
         USING STTNDSCD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   STTNDCDE,STTNDCDQ                                                
         MVI   STTNDSLN,STTNDSLQ                                                
         MVC   STTNDREP,STARCDE                                                 
         MVC   STTNDEFF,EFFDATE                                                 
         MVC   STTNDPRP,STAPREP                                                 
         MVC   STTNDCLS,STAOUTC                                                 
         MVC   STTNDFAX,STAFAX                                                  
         MVC   STTNDARB,MYABMKT                                                 
         MVC   STTNDNSI,MYNBMKT                                                 
         MVC   STTNMKTA,ALPHMKT                                                 
         MVC   STTNNEWS,STANEW     SAVE NEW STATION/BYPASS VALIDTN FLG          
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR80                                                             
         DC    H'0'                                                             
*                                                                               
VR80     DS    0H                  VALIDATE HOME MARKET CODES                   
         LA    R2,STAHMCH                                                       
*                                                                               
VR85     CLI   5(R2),0             NO INPUT, BUMP TO NEXT FIELD                 
         BE    VR120                                                            
*                                                                               
         CLI   5(R2),2             ONLY LOCATION CODE ENTERED                   
         BE    VR110                                                            
*                                                                               
VR90     DS    0H                  ENTIRE REP ID ENTERED                        
         CLI   5(R2),4             REP ID AT LEAST 4 CHARS.                     
         BL    INVLFLD                                                          
*                                                                               
         MVC   REPPREFX,8(R2)                                                   
         MVC   CITYCODE,11(R2)                                                  
         CLI   5(R2),5             2 OR 3 CHARACTER PREFIX??                    
         BE    VR99                                                             
         MVC   REPPREFX,SPACES                                                  
         MVC   REPPREFX(2),8(R2)                                                
         MVC   CITYCODE,10(R2)                                                  
*                                                                               
VR99     L     R4,AREPTAB          POINT TO REP TABLE                           
*                                                                               
VR100    CLI   0(R4),0             END OF TABLE??                               
         BE    INVLFLD             CAN'T FIND PREFIX                            
         CLC   REPPREFX,12(R4)     COMPARE PREFIX                               
         BE    VR105                                                            
VR103    ZIC   R3,0(R4)            GET LENGTH OF REP ENTRY                      
         LA    R4,0(R3,R4)         BUMP TO NEXT ROW                             
         B     VR100                                                            
*                                                                               
VR105    DS    0H                                                               
         CLI   1(R4),0             NOTHING TO COMPARE, TAKE ANYTHING            
         BE    VR110                                                            
         ZIC   R1,1(R4)            MOVE IN NUMBER OF LOCATION CODES             
         LA    R4,15(R4)           BUMP TO LOCATION CODES                       
         SR    R3,R3                                                            
*                                                                               
VR108    CLC   CITYCODE,0(R4)      COMPARE CITY CODE                            
         BE    VR110                                                            
         LA    R4,2(R4)            BUMP TO NEXT LOCATION CODE                   
         LA    R3,1(R3)                                                         
         CR    R3,R1               ALL DONE?                                    
         BL    VR108                                                            
         B     INVLFLD                                                          
*                                                                               
VR110    DS    0H                  ADD HOME MARKET ELEMENT                      
         LA    R6,ELEM                                                          
         USING STTNHMED,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   STTNHCDE,STTNHCDQ                                                
         MVI   STTNHMLN,STTNHMLQ                                                
*                                                                               
         OC    8(L'STTNHMKT,R2),SPACES                                          
         MVC   STTNHMKT,8(R2)                                                   
         DROP  R6                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR120    DS    0H                                                               
         LA    RF,STAHMCLH         CHECKED ALL FIELDS??                         
         CR    R2,RF                                                            
         BNL   VRX                                                              
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VR85                                                             
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* *******************************************************************           
* VALMKT- VALIDATE ALPHA MKT AGAINST FILE                                       
* *******************************************************************           
*                                                                               
VALMKT   NTR1                                                                   
         SPACE 1                                                                
         MVC   SVKEY,KEY           ORIG KEY                                     
         XC    KEY,KEY                                                          
         L     R6,AIO              PT TO STATION RECD                           
         USING STTNDSCD,R6                                                      
         LA    R4,KEY                                                           
         USING CTDMREC,R4                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,STAMED                                                  
         MVC   CTDMKMKT,ALPHMKT                                                 
         MVI   CTDMKSRC,C'A'       FIRST TRY ARB                                
         MVC   MYWORK(L'KEY),KEY                                                
*                                                                               
         MVC   AIO,AIO3            READ RECD INTO AIO3                          
         GOTO1 HIGH                                                             
         L     R1,AIO3                                                          
         CLC   MYWORK(22),0(R1)                                                 
         BE    VALMK10                                                          
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEY,MYWORK                                                       
         MVI   CTDMKSRC,C'N'       THEN TRY NSI                                 
         MVC   MYWORK,KEY                                                       
         GOTO1 HIGH                                                             
         L     R1,AIO3                                                          
         CLC   MYWORK(22),0(R1)                                                 
         BE    VALMK10                                                          
         XC    ALPHMKT,ALPHMKT                                                  
*                                                                               
VALMK10  MVC   KEY,SVKEY           RESTORE ORIG KEY                             
         GOTO1 HIGH                                                             
         ST    R6,AIO              RESTORE PTR TO RECD BEING BUILT              
*                                                                               
VALMKTX  OC    ALPHMKT,ALPHMKT                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         DROP  R6                                                               
* *******************************************************************           
* DR -   DISPLAY RECORD                                                         
* *******************************************************************           
*                                                                               
DR       DS    0H                                                               
         XC    SVABMKT,SVABMKT                                                  
         XC    SVNBMKT,SVNBMKT                                                  
         XC    SVREP,SVREP                                                      
         XC    SVMKTA,SVMKTA                                                    
         XC    STATION,STATION                                                  
*                                                                               
         LA    R4,STARCDEH                                                      
         TWAXC (R4)                CLEAR SCRN FIELDS                            
         CLI   STAMED,C'R'         FOR RADIO:                                   
         BNE   DR03                                                             
         NI    STAMKTAH+6,X'DF'    UNPROTECT ALPHA MKT                          
         OI    STAAMCH+6,X'20'     PROTECT ARB/NSI FIELDS                       
         OI    STANMCH+6,X'20'                                                  
         B     DR04                                                             
*                                                                               
DR03     DS    0H                  FOR TV:                                      
         OI    STAMKTAH+6,X'20'    PROTECT ALPHA MKT                            
         NI    STAAMCH+6,X'DF'     UNPROTECT ARB/NSI FIELDS                     
         NI    STANMCH+6,X'DF'                                                  
*                                                                               
DR04     L     R6,AIO                                                           
         USING STTNDSCD,R6                                                      
         MVC   STATION,STTNCALL-STTNRECD(R6)  SAVE STATION CALL LETTERS         
         MVI   ELCODE,STTNDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         LA    R4,STARCDEH                                                      
         MVC   STARCDE,STTNDREP                                                 
         MVC   SVREP,STTNDREP      SAVE OLD REP CODE                            
         FOUT  (R4)                                                             
*                                                                               
         OC    STTNDEFF,STTNDEFF   NO DATE, SKIP THIS FIELD                     
         BZ    DR05                                                             
         LA    R4,STADATEH                                                      
         GOTO1 DATCON,DMCB,(3,STTNDEFF),(11,8(R4))                              
         FOUT  (R4)                                                             
*                                                                               
DR05     LA    R4,STAPREPH                                                      
         MVC   STAPREP,STTNDPRP                                                 
         FOUT  (R4)                                                             
*                                                                               
         LA    R4,STAOUTCH                                                      
         MVC   STAOUTC,STTNDCLS                                                 
         FOUT  (R4)                                                             
*                                                                               
         LA    R4,STAFAXH                                                       
         MVC   STAFAX,STTNDFAX                                                  
         FOUT  (R4)                                                             
*                                                                               
         LA    R4,STANEWH                                                       
         MVC   STANEW,STTNNEWS                                                  
         FOUT  (R4)                                                             
*                                                                               
         CLI   STAMED,C'R'         FIELD VALID FOR RADIO ONLY                   
         BNE   DR07                                                             
         LA    R4,STAMKTAH                                                      
         MVC   STAMKTA,STTNMKTA    MOVE INTO SCREEN FIELD                       
         MVC   SVMKTA,STTNMKTA                                                  
         FOUT  (R4)                                                             
         B     DR10                BYPASS ARB/NSI MKT #'S                       
*                                                                               
DR07     MVC   SVABMKT,STTNDARB   SAVE OLD ARB RATING SERV MKT CODE             
         LA    R4,STAAMCH                                                       
         EDIT  (B2,STTNDARB),(4,8(R4)),ALIGN=LEFT                               
         FOUT  (R4)                                                             
*                                                                               
         MVC   SVNBMKT,STTNDNSI   SAVE OLD NSI RATING SERV MKT CODE             
         LA    R4,STANMCH                                                       
         EDIT  (B2,STTNDNSI),(4,8(R4)),ALIGN=LEFT                               
         FOUT  (R4)                                                             
         DROP  R6                                                               
*                                                                               
DR10     DS    0H                  DISPLAY HOME MARKET CODES                    
         LA    R4,STAHMCH                                                       
*                                                                               
         L     R6,AIO                                                           
         USING STTNHMED,R6                                                      
         MVI   ELCODE,STTNHCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
DR20     MVC   8(5,R4),STTNHMKT                                                 
         FOUT  (R4)                                                             
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
DR30     DS    0H                                                               
         LA    RF,STAHMCLH         DISPLAYED ALL FIELDS??                       
         CR    R4,RF                                                            
         BNL   DRX                                                              
*                                                                               
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         B     DR20                                                             
*                                                                               
DRX      DS    0H                                                               
         MVC   SVMED,STAMED        SAVE OLD MEDIA                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ******************************************************************            
* DK -   DISPLAY KEY                                                            
* ******************************************************************            
*                                                                               
DK       DS    0H                                                               
         XC    SVMED,SVMED                                                      
         XC    STATION,STATION                                                  
         MVC   SAVEKEY,KEY                                                      
         LA    R6,KEY                                                           
         USING STTNKEY,R6                                                       
*                                                                               
         LA    R4,STAMEDH                                                       
         MVI   STAMED,C'T'                                                      
         CLI   STTNCALL+4,C'T'                                                  
         BE    DK10                                                             
         MVI   STAMED,C'R'                                                      
*                                                                               
DK10     DS    0H                                                               
         FOUT  (R4)                                                             
         LA    R4,STAIDH                                                        
         XC    STAID,STAID                                                      
         MVC   STAID(4),STTNCALL                                                
         MVC   STATION,STTNCALL                                                 
         CLI   STAID+3,C' '                                                     
         BNE   DK20                                                             
         MVI   STAID+3,C'-'                                                     
         MVC   STAID+4(1),STTNCALL+4                                            
         B     DKX                                                              
*                                                                               
DK20     DS    0H                                                               
         MVI   STAID+4,C'-'                                                     
         MVC   STAID+5(1),STTNCALL+4                                            
*                                                                               
DKX      DS    0H                                                               
         FOUT  (R4)                                                             
         MVC   SVMED,STAMED                                                     
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* *********************************************************************         
* XP- ADD PASSIVE RECORD IF STATION RECD HAS BEEN CHANGED                       
* *********************************************************************         
*                                                                               
XP       DS    0H                                                               
         L     R4,AIO                                                           
         USING STTNRECD,R4         NEW RECORD THAT WAS JUST CHANGED             
*                                                                               
XP10     DS    0H                                                               
         LA    R4,KEY              SEE IF OLD PASSIVE RECD EXISTS               
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,SVMED                                                    
         MVC   STTNREP,SVREP                                                    
         MVC   STTNSTAC,STATION                                                 
         MVI   STTNRSRV,C'A'                                                    
         MVC   STTNRSMK,SVABMKT                                                 
         CLI   STAMED,C'R'         RADIO GETS PASSIVE RECD W/ALPHA CODE         
         BNE   *+10                                                             
         MVC   STTNAMKT,SVMKTA     ALPHA MKT CODE                               
         MVC   SVKEY(25),KEY                                                    
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XP20                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         CLC   SVMED,STAMED        ANY CHANGE IN KEY? NEW=OLD?                  
         BNE   XP17                                                             
         CLC   SVREP,STARCDE                                                    
         BNE   XP17                                                             
*                                                                               
         CLI   STAMED,C'R'         DIFF KEY FOR RADIO PASSIVE RECD              
         BNE   XP15                                                             
         CLC   SVMKTA,MYAMKT                                                    
         BNE   XP17                                                             
         B     XP16                                                             
*                                                                               
XP15     CLC   SVABMKT,MYABMKT     FOR TV, TEST ARB CODE FIELD                  
         BNE   XP17                                                             
*                                                                               
XP16     L     R1,AIO1                                                          
         MVC   25(250,R4),25(R1)   IF SAME,COPY NEW DATA AND WRITE OUT          
         B     XP18                                                             
*                                                                               
XP17     OI    STTNSTAT,X'80'      ELSE MARK FOR DELETION                       
         GOTO1 WRITE                                                            
         BE    XP20                                                             
         DC    H'0'                                                             
*                                                                               
XP18     GOTO1 WRITE                                                            
         BE    XP50                                                             
         DC    H'0'                                                             
*                                                                               
XP20     DS    0H                  SEE IF NEW PASSIVE REC EXISTS                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVC   STTNSTAC,STATION                                                 
         MVI   STTNRSRV,C'A'                                                    
         MVC   STTNRSMK,MYABMKT                                                 
         CLI   STAMED,C'R'         RADIO GETS PASSIVE RECD W/ALPHA CODE         
         BNE   *+10                                                             
         MVC   STTNAMKT,MYAMKT     ALPHA MKT CODE                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XP30                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         L     R1,AIO1                                                          
         MVC   25(250,R4),25(R1)   COPY NEW DATA                                
         GOTO1 WRITE                                                            
         BE    XP50                                                             
         DC    H'0'                                                             
*                                                                               
XP30     DS    0H                                                               
         MVC   AIO,AIO1            NO DUPLICATES SO ADD NEW RECORD              
         L     R4,AIO              CHANGE REC INTO PASSIVE RECORD               
         USING STTNRECD,R4                                                      
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVC   STTNSTAC,STATION                                                 
         MVI   STTNRSRV,C'A'       FOR TV ONLY                                  
         MVC   STTNRSMK,MYABMKT    FOR TV ONLY                                  
         CLI   STAMED,C'R'                                                      
         BNE   *+10                                                             
         MVC   STTNAMKT,MYAMKT     FOR RADIO ONLY                               
         MVC   KEY(25),0(R4)                                                    
         MVI   RDUPDATE,C'N'                                                    
XP40     GOTO1 ADD                                                              
         BE    XP50                                                             
         DC    H'0'                                                             
*                                                                               
XP50     CLI   STAMED,C'R'         IF RADIO DON'T BUILD NEILSON RECD            
         BE    XPXIT                                                            
*                                                                               
         LA    R4,KEY              SEE IF OLD PASSIVE RECD EXISTS               
         XC    KEY,KEY             FOR NEILSON                                  
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,SVMED                                                    
         MVC   STTNREP,SVREP                                                    
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,SVNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   SVKEY(25),KEY                                                    
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XP70                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         CLC   SVMED,STAMED        ANY CHANGE IN KEY? NEW=OLD?                  
         BNE   XP67                                                             
         CLC   SVREP,STARCDE                                                    
         BNE   XP67                                                             
         CLC   SVNBMKT,MYNBMKT                                                  
         BNE   XP67                                                             
         L     R1,AIO1                                                          
         MVC   25(250,R4),25(R1)   IF SAME,COPY NEW DATA AND WRITE OUT          
         B     XP68                                                             
*                                                                               
XP67     OI    STTNSTAT,X'80'      ELSE MARK FOR DELETION                       
         GOTO1 WRITE                                                            
         BE    XP70                                                             
         DC    H'0'                                                             
*                                                                               
XP68     GOTO1 WRITE                                                            
         BE    XPXIT                                                            
         DC    H'0'                                                             
*                                                                               
XP70     DS    0H                  SEE IF NEW PASSIVE REC EXISTS                
         LA    R4,KEY              FOR NEILSON                                  
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,MYNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XP80                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         L     R1,AIO1                                                          
         MVC   25(250,R4),25(R1)   COPY NEW DATA                                
         GOTO1 WRITE                                                            
         BE    XPXIT                                                            
         DC    H'0'                                                             
*                                                                               
XP80     DS    0H                                                               
         MVC   AIO,AIO1            NO DUPLICATES SO ADD NEW RECORD              
         L     R4,AIO              CHANGE REC INTO PASSIVE RECORD               
         USING STTNRECD,R4                                                      
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,MYNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   KEY(25),0(R4)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 ADD                                                              
         BE    XPXIT                                                            
         DC    H'0'                                                             
*                                                                               
XPXIT    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* XA  - ADD PASSIVE RECORD AFTER RECORD HAS BEEN ADDED                          
* *********************************************************************         
*                                                                               
XA       DS    0H                                                               
         USING STTNRECD,R4                                                      
*                                                                               
XA10     DS    0H                                                               
         LA    R4,KEY              SEE IF OLD PASSIVE RECD EXISTS               
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVC   STTNSTAC,STATION                                                 
         MVI   STTNRSRV,C'A'       FOR TV                                       
         MVC   STTNRSMK,MYABMKT    FOR TV                                       
         CLI   STAMED,C'R'                                                      
         BNE   *+10                                                             
         MVC   STTNAMKT,MYAMKT     ALPHA MKT CODE- FOR RADIO                    
         MVC   SVKEY(25),KEY                                                    
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'      READ THE DELETED RECORD                       
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XA30                                                             
         L     R4,AIO2                                                          
         USING STTNRECD,R4                                                      
         L     R1,AIO1             RECORD JUST ADDED IS IN AIO1                 
         MVC   25(250,R4),25(R1)   COPY NEW DATA                                
         GOTO1 WRITE                                                            
         BE    XA50                                                             
         DC    H'0'                                                             
*                                                                               
XA30     DS    0H                                                               
         MVC   AIO,AIO1            NO DUPLICATES SO ADD NEW RECORD              
         L     R4,AIO              CHANGE REC INTO PASSIVE RECORD               
         USING STTNRECD,R4                                                      
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVC   STTNSTAC,STATION                                                 
         MVI   STTNRSRV,C'A'       FOR TV ONLY                                  
         MVC   STTNRSMK,MYABMKT    FOR TV ONLY                                  
         CLI   STAMED,C'R'                                                      
         BNE   *+10                                                             
         MVC   STTNAMKT,MYAMKT     ALPHA MKT FOR RADIO ONLY                     
         MVC   KEY(25),0(R4)                                                    
         GOTO1 ADD                                                              
         BE    XA50                                                             
         DC    H'0'                                                             
*                                                                               
XA50     DS    0H                                                               
         CLI   STAMED,C'R'                                                      
         BE    XAXIT                                                            
         LA    R4,KEY              SEE IF OLD PASSIVE RECD EXISTS               
         XC    KEY,KEY             FOR NEILSON                                  
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,MYNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   SVKEY(25),KEY                                                    
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'      READ THE DELETED RECORD                       
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XA80                                                             
         L     R4,AIO2                                                          
         USING STTNRECD,R4                                                      
         L     R1,AIO1             RECORD JUST ADDED IS IN AIO1                 
         MVC   25(250,R4),25(R1)   COPY NEW DATA                                
         GOTO1 WRITE                                                            
         BE    XAXIT                                                            
         DC    H'0'                                                             
*                                                                               
XA80     DS    0H                                                               
         MVC   AIO,AIO1            NO DUPLICATES SO ADD NEW RECORD              
         L     R4,AIO              CHANGE REC INTO PASSIVE RECORD               
         USING STTNRECD,R4                                                      
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,STARCDE                                                  
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,MYNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   KEY(25),0(R4)                                                    
*                                                                               
         GOTO1 ADD                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XAXIT    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* XD -   DELETE PASSIVE KEY FOR THE RECD AFTER IT HAS BEEN DELETED              
* *********************************************************************         
*                                                                               
XD       DS    0H                                                               
         L     R6,AIO                                                           
         USING STTNDSCD,R6                                                      
         MVI   ELCODE,STTNDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         MVC   SVREP,STTNDREP      SAVE OLD REP CODE                            
         MVC   SVABMKT,STTNDARB    SAVE OLD ARB RATING SERV MKT CODE            
         MVC   SVNBMKT,STTNDNSI    SAVE OLD NSI RATING SERV MKT CODE            
         MVC   SVMKTA,STTNMKTA     SAVE ALPHA MKT CODE                          
*                                                                               
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
*                                                                               
XD10     DS    0H                  SEE IF NEW PASSIVE RECD EXISTS               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,SVREP       SAVE OLD REP CODE                            
         MVI   STTNRSRV,C'A'       FOR TV ONLY                                  
         MVC   STTNRSMK,SVABMKT    DO ARB FIRST FOR TV                          
         CLI   STAMED,C'R'                                                      
         BNE   *+10                                                             
         MVC   STTNAMKT,SVMKTA     FOR RADIO, KEY HAS ALPHA MKT                 
         MVC   STTNSTAC,STATION                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XD50                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         OI    STTNSTAT,X'80'      MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         BE    XD50                                                             
         DC    H'0'                                                             
*                                                                               
XD50     DS    0H                  DO FOR NSI                                   
         CLI   STAMED,C'R'                                                      
         BE    XDXIT                                                            
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,SVREP       SAVE OLD REP CODE                            
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,SVNBMKT    DO ARB FIRST- FOR TV                         
         MVC   STTNSTAC,STATION                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XDXIT                                                            
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         OI    STTNSTAT,X'80'      MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         BE    XDXIT                                                            
         DC    H'0'                                                             
*                                                                               
XDXIT    B     EXIT                                                             
         DROP  R6                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* XR  -  RESTORE PASSIVE KEY FOR THE REC AFTER IT HAS BEEN RESTORED             
* *********************************************************************         
*                                                                               
XR       DS    0H                                                               
         L     R6,AIO                                                           
         USING STTNDSCD,R6                                                      
         MVI   ELCODE,STTNDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         MVC   SVREP,STTNDREP      SAVE OLD REP CODE                            
         MVC   SVMKTA,STTNMKTA     SAVE OLD ALPHA MKT CODE                      
         MVC   SVABMKT,STTNDARB    SAVE OLD ARB RATING SERV MKT CODE            
         MVC   SVNBMKT,STTNDNSI    SAVE OLD NSI RATING SERV MKT CODE            
*                                                                               
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
*                                                                               
XR10     DS    0H                  RESTORE ARB RECORD                           
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,SVREP                                                    
         MVI   STTNRSRV,C'A'                                                    
         MVC   STTNRSMK,SVABMKT                                                 
         CLI   STAMED,C'R'                                                      
         BNE   *+10                                                             
         MVC   STTNAMKT,SVMKTA                                                  
         MVC   STTNSTAC,STATION                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ THE DELETED RECORD                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XR50                                                             
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         NI    STTNSTAT,X'FF'-X'80' SET DELETED BIT OFF                         
         GOTO1 WRITE                                                            
         BE    XR50                                                             
         DC    H'0'                                                             
*                                                                               
XR50     DS    0H                  RESTORE NSI RECORD- FOR TV ONLY              
         CLI   STAMED,C'R'                                                      
         BE    XRXIT                                                            
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,STAMED                                                   
         MVC   STTNREP,SVREP                                                    
         MVI   STTNRSRV,C'N'                                                    
         MVC   STTNRSMK,SVNBMKT                                                 
         MVC   STTNSTAC,STATION                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ THE DELETED RECORD                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   XRXIT                                                            
         L     R4,AIO                                                           
         USING STTNRECD,R4                                                      
         NI    STTNSTAT,X'FF'-X'80' SET DELETED BIT OFF                         
         GOTO1 WRITE                                                            
         BE    XRXIT                                                            
         DC    H'0'                                                             
*                                                                               
XRXIT    B     EXIT                                                             
         EJECT                                                                  
         DROP  R4                                                               
         DROP  R6                                                               
*                                                                               
* *********************************************************************         
* ONLINE LIST ROUTINE                                                           
* *********************************************************************         
*                                                                               
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
*                                                                               
         LA    R3,HEADING          SET UP REPORT HEADINGS                       
         ST    R3,SPECS                                                         
         LA    R3,HDRTN                                                         
         ST    R3,HEADHOOK                                                      
         LA    R5,P                                                             
*                                                                               
LR02     DS    0H                                                               
         LA    R4,KEY                                                           
         USING STTNKEY,R4                                                       
*                                                                               
         CLI   BITFLAG,0           KEY OR FILTERS CHANGED?                      
         BNE   LR05                YES                                          
*                                                                               
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SAVEKEY         USE SELECTED KEY                             
         XC    SAVEKEY,SAVEKEY                                                  
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'STTNKEY),KEY   FIRST TIME THROUGH?                         
         BNZ   LR10                                                             
         MVI   STTNSYS,STTNSYSQ    X'0506'                                      
         MVI   STTNTYP,STTNTYPQ    STATION REC                                  
         MVC   STTNCALL,STAID                                                   
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR40                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR40     CLI   STTNSYS,STTNSYSQ    X'0506'                                      
         BNE   *+12                                                             
         CLI   STTNTYP,STTNTYPQ    STATION RECORD KEY                           
         BE    LR41                                                             
         XC    SAVEKEY,SAVEKEY     CLEAR LAST KEY WE GOT                        
         B     LRX                                                              
*                                                                               
LR41     CLI   LSTMED,C'T'                                                      
         BE    LR43                                                             
         CLI   STTNCALL+4,C'T'     MEDIA FILTER                                 
         BE    LRSEQ                                                            
         B     LR44                                                             
LR43     CLI   STTNCALL+4,C'T'                                                  
         BNE   LRSEQ                                                            
*                                                                               
LR44     DS    0H                                                               
         CLI   LSTREPH+5,0                                                      
         BE    LR45                                                             
*                                                                               
         ZIC   RF,LSTREPH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   STTNDREP(0),LSTREP                                               
         BNE   LRSEQ                                                            
*                                                                               
LR45     DS    0H                                                               
         CLI   LSTDATH+5,0         DATE RECD CHANGED FILTER                     
         BE    LR46                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   LRSEQ               DOESN'T EXIST->RECD NOT CHGD LATELY          
         CLC   2(3,R6),THISDATE    WAS RECD CHANGED ON FILTER DATE              
         BNE   LRSEQ               NO--DON'T DISPLAY                            
*                                                                               
LR46     CLI   LSTHOMEH+5,0                                                     
         BE    LR46A                                                            
         LA    R2,LSTHOMEH                                                      
         CLI   LSTHOME,C'N'                                                     
         BNE   INVLFLD                                                          
         L     R6,AIO              ONLY DISP RECS W/OUT HOME MKT ELEM           
         MVI   ELCODE,STTNDCDQ     HOME MKT ELEMET                              
         BAS   RE,GETEL                                                         
         BNE   LR65                NO DESCR ELEMNT FOUND                        
         USING STTNDSCD,R6                                                      
         OC    STTNMKTA,STTNMKTA   ALPHA MKT ON THIS RECD?                      
         BNZ   LRSEQ               YES, IGNORE                                  
*                                                                               
LR46A    MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   PSTAID,STTNCALL                                                  
*                                                                               
         L     R6,AIO                                                           
         USING STTNDSCD,R6                                                      
         MVI   ELCODE,STTNDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR65                                                             
*                                                                               
         MVC   PREPCD,STTNDREP                                                  
         OC    STTNDEFF,STTNDEFF   NO DATE, SKIP THIS FIELD                     
         BZ    LR47                                                             
         GOTO1 DATCON,DMCB,(3,STTNDEFF),(11,PEFFDT)                             
*                                                                               
LR47     MVC   PPVREP,STTNDPRP                                                  
         MVC   PALPHA,STTNMKTA                                                  
         EDIT  (B2,STTNDARB),(4,PARBCD),ALIGN=LEFT                              
         EDIT  (B2,STTNDNSI),(4,PNSICD),ALIGN=LEFT                              
         MVC   POUTCL,STTNDCLS                                                  
         MVC   PFAXNO,STTNDFAX                                                  
         DROP  R6                                                               
*                                                                               
LR65     CLI   MODE,PRINTREP                                                    
         BE    LR70                GO PRINT REPORT                              
         MVC   SAVEKEY,KEY         SAVE LAST KEY WE GOT                         
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LRSEQ                                                            
*                                                                               
* PRINT REPORT ROUTINE  * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
LR70     DS    0H                                                               
*** MOVE IN REST OF REP OFFICES                                                 
***      MVC   P+10(L'PSTAID),PSTAID       KEY DATA FROM LIST TO P              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ                                                            
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
* SET UP VARIABLES POINTING TO APPROPRIATE REP TABLES * * * * * * * *           
*                                                                               
SETREP   NTR1                                                                   
         LA    RF,TREPTAB                                                       
         ST    RF,AREPTAB          GET ADD OF TV REP CODE TABLE                 
*                                                                               
         LA    R2,STAMEDH                                                       
         CLI   8(R2),C'T'          MEDIA MUST BE T/R                            
         BE    SETREPX                                                          
*                                  ASSUME MEDIA R, VK WILL VERIFY               
         LA    RF,RREPTAB                                                       
         ST    RF,AREPTAB          GET ADD OF RADIO REP CODE TABLE              
*                                                                               
SETREPX  B     EXIT                                                             
*                                                                               
* HEADER ROUTINE    * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,25,C'ADDS STATION RECORDS'                                    
         SSPEC H2,25,C'--------------------'                                    
         SSPEC H1,55,AGYNAME                                                    
         SSPEC H2,55,AGYADD                                                     
         SSPEC H3,55,REPORT                                                     
         SSPEC H4,55,RUN                                                        
         SSPEC H5,55,PAGE                                                       
         DC    X'0'                                                             
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         LA    R4,H8                                                            
         USING PLINED,R4                                                        
         MVC   PSTAID(33),=C'STATION  REP  EFFDT     PREV  ARB'                 
         MVC   PNSICD(30),=C'NSI   ALP MKT  CLS  FAX NUMBER'                    
         LA    R4,H9                                                            
         MVC   PSTAID(33),=C'-------  ---  -----     ----  ---'                 
         MVC   PNSICD(30),=C'---   -------  ---  ----------'                    
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
MISSFLD  MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
INVLMED  MVC   GERROR,=AL2(INVMED)                                              
         B     VSFMERR                                                          
*                                                                               
INVLSTA  MVC   GERROR,=AL2(INVSTA)                                              
         B     VSFMERR                                                          
*                                                                               
NOAUTHC  MVC   GERROR,=AL2(NOAUTHCH)                                            
         B     VSFMERR                                                          
*                                                                               
NOAUTHS  MVC   GERROR,=AL2(NOAUTHSH)                                            
         B     VSFMERR                                                          
*                                                                               
BADSWTC  MVC   GERROR,=AL2(BADSWTCH)                                            
         B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
***                                                                             
       ++INCLUDE SPADREPS                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENADSTA                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC7D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0A1C STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
MYWORK   DS    XL96                                                             
FLAG     DS    X                                                                
BITFLAG  DS    X                   BITS FOR WHICH KEY/FILTER CHANGED            
SVKEY    DS    XL48                ADDS STATION RECORD KEY                      
SAVEKEY  DS    XL48                                                             
AREPTAB  DS    A                   ADDRESS OF REP TABLE                         
REPPREFX DS    CL3                 REP PREFIX                                   
CITYCODE DS    CL2                 CITY CODE                                    
EFFDATE  DS    XL3                 EFFECTIVE DATE                               
THISDATE DS    XL3                 TODAY'S DATE                                 
MYABMKT  DS    H                   ARB BIN MKT                                  
MYNBMKT  DS    H                   NSI BIN MKT                                  
MYAMKT   DS    CL3                 ALPHA MKT                                    
ALPHMKT  DS    CL3                 ALPHA MKT FOR RADIO                          
SVABMKT  DS    H                                                                
SVNBMKT  DS    H                                                                
STATION  DS    CL5                                                              
SVMED    DS    CL1                                                              
SVREP    DS    CL3                                                              
SVMKTA   DS    CL3                                                              
*                                                                               
* ONLINE LIST LINE   * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
*SEL  STATION  REP  EFFDT  PREP  ARB   NSI   ALP MKT  CLS  FAX NUMBER           
*                                                                               
PLINED   DSECT                                                                  
PSTAID   DS    CL7                                                              
         DS    CL2                                                              
PREPCD   DS    CL3                                                              
         DS    CL2                                                              
PEFFDT   DS    CL8                                                              
         DS    CL2                                                              
PPVREP   DS    CL3                                                              
         DS    CL3                                                              
PARBCD   DS    CL4                                                              
         DS    CL2                                                              
PNSICD   DS    CL4                                                              
         DS    CL2                                                              
PALPHA   DS    CL3                                                              
         DS    CL6                                                              
POUTCL   DS    CL1                                                              
         DS    CL4                                                              
PFAXNO   DS    CL14                                                             
PEND     EQU   *                                                                
*                                                                               
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118CTSFM1C   05/01/02'                                      
         END                                                                    
