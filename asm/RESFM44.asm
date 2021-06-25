*          DATA SET RESFM44    AT LEVEL 149 AS OF 08/29/00                      
*PHASE T81844A                                                                  
         TITLE 'T81844 - RESFM44 - CUME OVERRIDE'                               
***********************************************************************         
*                                                                     *         
*  RESFM44 (T81844) --- INPUT OF CUME OVERRIDE BOOKS BY MARKET/QUARTER*         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 04AUG00 (FJD) DATE OF BIRTH                                         *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T81844   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81844*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
                                                                                
DB       USING DBLOCK,MYDBLOCK                                                  
                                                                                
         MVI   ACTELOPT,C'N'       WE HANDLE AUDIT TRAIL                        
         OI    GENSTAT4,NODELLST   DON'T ALLOW DELETE IN LIST                   
                                                                                
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY?                                
         BNE   MAIN20                                                           
         BRAS  RE,VK                                                            
         B     EXIT                                                             
                                                                                
MAIN20   CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BNE   MAIN30                                                           
         BRAS  RE,DK                                                            
         B     EXIT                                                             
                                                                                
MAIN30   CLI   MODE,VALREC         VALIDATE RECORD?                             
         BNE   MAIN40                                                           
         BRAS  RE,VR                                                            
         BRAS  RE,DR                                                            
         B     EXIT                                                             
                                                                                
MAIN40   CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BNE   MAIN50                                                           
         BRAS  RE,DR                                                            
         B     EXIT                                                             
                                                                                
MAIN50   CLI   MODE,LISTRECS       LIST RECORDS?                                
         BNE   EXIT                                                             
         BRAS  RE,LR                                                            
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*       VALIDATE THE KEY                                                        
***********************************************************************         
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    RECID,RECID         CLEAR KEY HOLDER                             
         XC    MKTINFO,MKTINFO     CLEAR SPACE FOR 'DEFINE' RETURN              
         BRAS  RE,INITDBLK         CLEAR AND INIT DBLOCK                        
                                                                                
         XC    CUMMKTN,CUMMKTN     ENSURE PREVIOUSLY DISPLAYED                  
         OI    CUMMKTNH+6,X'80'     MKTNAME CLEARED AND XMIT                    
                                                                                
         MVC   REPALPHA,AGENCY     PLACE REP CODE IN KEY HOLDER                 
                                                                                
         LA    R2,CUMMKTCH         VALIDATE MARKET CODE                         
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK0010              YES                                          
                                                                                
         LA    R2,CUMSTATH         NO, CHECK STATION FLD                        
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK0005              YES                                          
         CLI   ACTNUM,ACTLIST      NO, CHECK IF LIST REQUEST                    
         BE    VKBLD                   YES, FULL KEY NOT NEEDED                 
         CLI   ACTNUM,ACTREP           NO, CHECK IF REPORT REQUEST              
         BE    VKBLD                       YES, FULL KEY NOT NEEDED             
         J     MISSFLD                     NO, ERROR-MUST HAVE AN INPUT         
*                                                                               
*           GET MARKET FROM STATION FIELD INPUT                                 
*                                                                               
VK0005   MVC   DB.DBSELSTA,CUMSTAT                                              
         CLI   DB.DBSELSTA+4,C' '  IF SPACE IN MEDIA BYTE                       
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'  THEN, DEFAULT TO 'T'V                        
                                                                                
         BRAS  RE,CALLDEM                                                       
                                                                                
         OC    DB.DBACTRMK,DB.DBACTRMK     DID THIS STATION FIND INFO?          
         BNZ   *+8                                                              
         J     INVLFLD                     NO, WAS NOT A VALID STATION          
**                                         YES, DEMAND GAVE US MKT INFO         
                                                                                
         MVC   CUMMKTN,MKTNAME                      PUT NAME ON SCREEN          
         OI    CUMMKTNH+6,X'80'                     TRANSMIT IT                 
         EDIT  (B2,MKTNUM),CUMMKTC,FILL=0       PUT MKT CODE ON SCREEN          
         OI    CUMMKTCH+6,X'80'                 TRANSMIT IT                     
         MVC   MKTALPHA(L'CUMMKTC),CUMMKTC      COPY IT INTO KEY HOLDER         
         MVI   MKTALPHA+4,C' '                  WITH TRAILNG SPACE CHAR         
         XC    CUMSTAT,CUMSTAT                  CLEAR STATION                   
         OI    CUMSTATH+6,X'80'                  AND XMIT                       
         B     VKBLD                            NOW BUILD KEY                   
                                                                                
*                                                                               
*      VALIDATE INPUT MARKET CODE AND FILL IN MARKET NAME ON SCREEN             
*                                                                               
VK0010   CLI   CUMSTATH+5,0        CAN ONLY ENTER MKT CODE OR STATION           
         JNE   MKTORSTA                NOT BOTH                                 
         TM    4(R2),VALIDNUM      IF NOT VALID NUMERIC ENTRY                   
         JNO   INVLFLD             THEN, ERROR                                  
                                                                                
         ZIC   R1,5(R2)            YES, GET INPUT LENGTH                        
         LTR   R1,R1                IF 0                                        
         JZ    INVLFLD               ERROR                                      
         BCTR  R1,0                  ELSE, DECREMENT FOR EX INSTR               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)              YES, PACK,                              
                                                                                
         CVB   R0,DUB                   CONVERT,                                
         STCM  R0,3,DB.DBSELRMK         AND STORE                               
         EDIT  (R0),CUMMKTC,FILL=0      REDISPLAY WITH LEADING 0'S              
         OI    CUMMKTCH+6,X'80'         AND XMIT                                
                                                                                
         BRAS  RE,CALLDEM                                                       
                                                                                
         MVC   CUMMKTN,MKTNAME   PUT RETURNED NAME ON SCRN -MAYBE NULL          
         OI    CUMMKTNH+6,X'80'  TRANSMIT IT                                    
                                                                                
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    VK0040              THEN SKIP NEXT 2 TESTS                       
                                                                                
****     FOLLOWING BLOCK TESTS IF CODE RELATES TO VALID MARKET                  
                                                                                
         OC    MKTINFO,MKTINFO     ELSE, DID DEMHOOK RETURN ANYTHING?           
         JZ    INVLFLD             NO, MUST BE BAD CODE                         
         CLC   =C'**UNKNOWN**',MKTNAME    IF NAME NOT KNOWN                     
         JE    INVLFLD                    THEN, BAD INPUT                       
                                                                                
                                                                                
VK0040   MVC   MKTALPHA(L'CUMMKTC),CUMMKTC   PUT IT IN KEY HOLDER               
         MVI   MKTALPHA+4,C' '               WITH A TRAILING SPACE CHAR         
*                                                                               
VKBLD    LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCUMKEY,R6                                                       
         MVI   RCUMTYP,RCUMTYQ     INIT GENERIC PORTION OF KEY                  
         MVI   RCUMSTY,RCUMSTQ                                                  
         MVC   RCUMREP,REPALPHA    PLACE UNIQUE PORTION OF KEY                  
         MVC   RCUMMKT,MKTALPHA      FROM SAVED KEYHOLDER                       
         DROP  R6                                                               
                                                                                
VKX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*         INITIALIZE DBLOCK FOR DEMAND CALLS                                    
***********************************************************************         
INITDBLK NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    DB.DBLOCK,DB.DBLOCK       CLEAR AND INIT DBLOCK FOR              
         MVC   DB.DBAREC,AIO2                  VALIDATION CALLS                 
         MVC   DB.DBSELAGY,AGENCY                                               
         MVC   DB.DBFILE,=C'TP '         TAPE=TIME PERIOD                       
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'          MEDIA=TELEVISION                       
         MVI   DB.DBSELSRC,C'N'          RATINGS SOURCE=NIELSEN                 
         MVI   DB.DBFUNCT,DBGETMK        GET MARKET RECORD                      
                                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*         SEPARATE ROUTINE FOR DEMAND CALL-- !ENSURES ADDRESSABILITY TO         
*            HOOK ROUTINE!                                                      
***********************************************************************         
CALLDEM  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DEMAND,DMCB,DB.DBLOCK,DEMHOOK1,0                                 
         XIT1                                                                   
*----------------------------------------------------------------------         
*         HOOK 1 FOR DEMAND CALLS                                               
*----------------------------------------------------------------------         
DEMHOOK1 NTR1                                                                   
         GOTO1 DEFINE,DMCB,=C'MNAME',DB.DBLOCK,MKTINFO                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*         VALIDATE THE RECORD ON SCREEN                                         
***********************************************************************         
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO                                                           
         USING RCUMKEY,R4                                                       
                                                                                
         MVI   ELCODE,RCUMOVEQ     OVERRIDE ELEMENT                             
         GOTO1 REMELEM                                                          
         XC    OVERTAB,OVERTAB     CLEAR HOLDING TABLE                          
         MVI   OVERTAB,X'FF'       PLACE EOT MARKER AT BEGINNING                
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK+8(5),=C'FEB  ' INIT MOCK SCREEN FIELD FOR BOOK              
         MVI   WORK+5,5                 LEAVING OUT YEAR.  WE WILL              
         MVI   WORK+0,13                USE BOOKVAL TO GET 1BYTE YR             
                                                                                
         LA    R5,OVERTAB          R5 POINTS AT CURRENT TABLE POSITION          
                                                                                
         LA    R3,CUMYEARH                                                      
         USING ENTRY,R3                                                         
                                                                                
VR0010   XC    OVERENT,OVERENT     CLEAR TEMP HOLDER                            
                                                                                
         LA    R2,ENTYEARH                                                      
         CLI   ENTYEARH+5,0         YEAR DATA ENTERED?                          
         BE    VR0040              NO, CHECK NEXT ENTRY                         
                                                                                
*       GET VALID 1 BYTE BINARY YEAR USING MOCK SCREENFLD IN WORK               
*           AND BOOKVAL (I KNOW... THIS IS A LITTLE CUMBERSOME)                 
                                                                                
         MVC   WORK+11(2),ENTYEAR  INSERT YEAR IN MOCK SCREEN FIELD             
         XC    DUB,DUB                                                          
                                                                                
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,DUB),SCANNER                         
         CLI   DMCB+4,1            BOOK VALID?                                  
         JNE   INVLFLD             NO, THEN YEAR MUST BE BAD                    
         MVC   OVYEAR,DUB+1        ELSE, PUT YEAR IN TEMP HOLDER                
                                                                                
         LA    R2,ENTQTRH          POINT R2 AT CURRENT TEST FIELD               
         CLI   ENTQTRH+5,0         ASSOCIATED QUARTER ENTERED?                  
         BNE   VR0020                YES                                        
         J     MISSFLD               NO, EXIT WITH ERROR                        
                                                                                
VR0020   CLI   ENTQTR,C'1'         IF QUARTER < THAN "1"                        
         JL    INVLFLD                THEN ERROR                                
         CLI   ENTQTR,C'4'         ELSE, IF QUARTER > "4"                       
         JH    INVLFLD                THEN ERROR                                
         MVC   OVQTR,ENTQTR        ENTERED QUARTER MUST BE OKAY                 
         NI    OVQTR,X'0F'         TURN OFF HIGH ORDER NIBBLE                   
                                                                                
         LA    R2,ENTBOOKH         POINT R2 AT CURRENT TEST FIELD               
         GOTO1 BOOKVAL,DMCB,(C'N',(R2)),(1,DUB),(C'B',SCANNER),BYTE             
         CLI   DMCB+4,1            VALID BOOK?                                  
         JNE   INVLFLD                                                          
         CLI   BYTE,0              IF NON-STANDARD BOOK                         
         JNE   STANDON             THEN ERROR-"STANDARD ONLY"                   
         TM    DUB,X'40'           IS NIELSEN FLAG ON?                          
         JNO   INVLFLD             NO,ERROR                                     
         TM    DUB,X'FF'-X'40'     ARE ALL OTHER FLAGS OFF?                     
         JNZ   STANDON             NO, ERROR(THEY SHOULD BE)                    
         MVC   OVBOOK,DUB                                                       
                                                                                
         LA    R2,ENTYEARH         POINT BACK AT BEGINNING OF ENTRY             
***                                 TO WARN OF DUPLICATE(IF APPLICABL)          
         BRAS  RE,CHKDUP           CHECK FOR DUPLICATE ENTRY                    
         JE    DUPLIC8             IF FOUND, ERROR                              
                                                                                
         MVC   0(L'OVERENT,R5),OVERENT  PLACE IN TABLE AT CURRENT LOC           
         LA    R5,L'OVERENT(R5)         BUMP TABLE POINTER                      
         MVI   0(R5),X'FF'              INSERT NEW EOT MARK                     
                                                                                
*            ADD THIS ELEMENT TO RECORD                                         
*                                                                               
         LA    R7,ELEM                                                          
         USING RCUMOVER,R7                                                      
         XC    ELEM,ELEM                                                        
         MVI   RCUMOVEC,RCUMOVEQ        OVERRIDE ELEMENT ID                     
         MVI   RCUMOVLN,RCUMLQ          OVERRIDE ELEMENT LENGTH                 
         MVC   RCUMOVYR,OVYEAR          ENTER YEAR IN ELEMENT                   
         MVC   RCUMOVQT,OVQTR           ENTER QUARTER IN ELEMENT                
         MVC   RCUMBOOK,OVBOOK          ENTER BOOK IN ELEMENT                   
         GOTO1 ADDELEM                                                          
         DROP  R7                                                               
                                                                                
VR0040   LA    R2,CUMENDL               R2 = LAST FIELD ON SCREEN               
         LA    R3,ENTRYLQ(R3)           BUMP R3 PAST CURRENT ENTRY              
         CR    R3,R2                    IF NOT AT END OF SCREEN                 
         BL    VR0010                   THEN CHECK FOR MORE ENTRIES             
                                                                                
                                                                                
                                                                                
         DROP  R3,R4                                                            
VRX      BRAS  RE,AUDIT                 SET AUDIT TRAIL, AND                    
         XIT1                            OUR WORK HERE IS DONE                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATES                                                          
***********************************************************************         
CHKDUP   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,OVERTAB                                                       
CHKD10   CLI   0(R1),X'FF'         HAVE WE ENCOUNTERED THE EOT?                 
         BE    NO                  IF SO, NO DUPLICATE FOUND                    
         CLC   OVERENT(OVBOOK-OVERENT),0(R1)  CHECK YEAR & QUARTER              
         BE    YES                                                              
         LA    R1,L'OVERENT(R1)                                                 
         B     CHKD10                                                           
NO       LA    R1,1                TABLE SCANNED WITHOUT MATCH                  
         B     *+6                                                              
YES      SR    R1,R1               ILLEGAL MATCH ON YEAR & QUARTER              
         LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R4,AIO                                                           
         USING RCUMKEY,R4                                                       
                                                                                
         MVC   CUMMKTC,RCUMMKT          MARKET CODE                             
         OI    CUMMKTCH+6,X'80'                                                 
                                                                                
         BRAS  RE,INITDBLK              MARKET NAME                             
         XC    DUB,DUB                                                          
         PACK  DUB,RCUMMKT(4)      CURRENTLY, 5TH CHAR IS ALWAYS A ' '          
         CVB   R0,DUB                                                           
         STCM  R0,3,DB.DBSELRMK                                                 
         BRAS  RE,CALLDEM          GET MARKET NAME                              
         MVC   CUMMKTN,MKTNAME                                                  
         OI    CUMMKTNH+6,X'80'                                                 
                                                                                
         XC    CUMSTAT,CUMSTAT          CLEAR STATION                           
         OI    CUMSTATH+6,X'80'         & XMIT                                  
                                                                                
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        AUDIT (AUDIT TRAIL MAINTAINANCE)                                       
***********************************************************************         
AUDIT    NTR1  BASE=*,LABEL=*                                                   
         MVC   BLAMEFLD,SPACES               CLEAR ACTION HOLDER                
         MVC   BLAMEFLD(L'CONACT),CONACT     INSERT ACTION FROM SCREEN          
                                                                                
**           CALL REPFACS  AND UPDATE AUDIT TRAIL                               
                                                                                
         GOTO1 (RFAUDIT,REPFACS),DMCB,AIO,(5,ACOMFACS),BLAMEFLD,(RA)            
                                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TWAXC CUMYEARH,CUMENDLH   CLEAR SCREEN                                 
                                                                                
         LA    R1,CUMENDL          POINT R1 AT END OF SCREEN                    
         LA    R2,CUMYEARH         POINT AT FIRST OVERRIDE FIELD                
         USING ENTRY,R2                                                         
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,RCUMOVEQ     OVERRIDE ELEMENT                             
         USING RCUMOVER,R6                                                      
         BRAS  RE,GETEL            LOOK FOR ELEMENT                             
         BE    *+8                                                              
         B     DRX                 END IF NO ELEMENT TO DISPLAY                 
                                                                                
                                                                                
                                                                                
*       TRANSLATE 1 BYTE ELEMENT YEAR TO TWO CHAR VALUE                         
                                                                                
DR0010   ZIC   R3,RCUMOVYR         1 BYTE YEAR                                  
         CHI   R3,100              IS IT > 100 (DENOTING POST Y2K)              
         BL    DR0020                     NO,                                   
         SHI   R3,100                     YES, REMOVE FOR DISPLAY               
DR0020   EDIT  (R3),ENTYEAR,FILL=0        PUT IT ON SCREEN                      
         OI    ENTYEARH+6,X'80'             XMIT                                
                                                                                
                                                                                
*       TRANSLATE 1 BYTE ELEMENT QUARTER FIELD TO CHAR VALUE                    
                                                                                
         EDIT  (B1,RCUMOVQT),ENTQTR,FILL=0     PUT QUARTER ON SCREEN            
         OI    ENTQTRH+6,X'80'                 TRANSMIT IT                      
                                                                                
*       TRANSLATE ELEMENT BOOK BYTES TO 5 CHAR TEXT DESC (IE: FEB00)            
                                                                                
         MVC   TMPBOOK,RCUMBOOK+1  HOLDS YEAR/MONTH BYTES                       
         BRAS  RE,TRANSBK          TRANSLATE SAVED BOOK IN WORK                 
         MVC   ENTBOOK,WORK        PUT ON SCREEN,                               
         OI    ENTBOOKH+6,X'80'    XMIT IT                                      
*                                                                               
         BRAS  RE,NEXTEL           MORE OVERRIDE ELEMENTS ON RECORD?            
         BNE   DRX                  NO,                                         
         LA    R2,ENTRYLQ(R2)       YES, BUMP TO NEXT OUTPUT FIELD              
         CR    R2,R1               ARE WE PAST END OF SCREEN?                   
         BL    DR0010               NO, ALL SEEMS WELL, REPEAT LOOP             
         DC    H'0'                 YES, SHOULD NEVER HAPPEN!                   
                                                                                
                                                                                
         DROP  R2,R6                                                            
*                                                                               
DRX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TRANSLATE SAVED BOOK TO TEXT                                                  
*           (CODE ADAPTED FROM ROVER MODULE RERMP18)                            
***********************************************************************         
TRANSBK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
* AT ENTRY,                                                                     
*   TMPBOOK = BOOK TO TRANSLATE.                                                
*   AT EXIT,                                                                    
*   WORK    = CONVERTED BOOK.                                                   
                                                                                
TRSLTBK  MVC   WORK,SPACES                                                      
                                                                                
         OC    TMPBOOK,TMPBOOK                                                  
         BZ    TBKX                                                             
                                                                                
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
                                                                                
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         MVC   3(2,R2),4(R2)       REMOVE SLASH                                 
         LA    R2,5(R2)                                                         
         MVI   0(R2),C' '                                                       
TBKX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* LIST THE RECORDS                                                              
***********************************************************************         
LR       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R4,KEY                                                           
         USING RCUMKEY,R4                                                       
         OC    KEY,KEY             FIRST TIME CALLED?                           
         BNZ   LR10                                                             
                                                                                
         MVI   RCUMTYP,RCUMTYQ     INIT KEY                                     
         MVI   RCUMSTY,RCUMSTQ                                                  
         MVC   RCUMREP,REPALPHA                                                 
         MVC   RCUMMKT,MKTALPHA                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
                                                                                
LR10     GOTO1 HIGH                GET 1ST RECORD                               
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
                                                                                
LR20     GOTO1 SEQ                 GET NEXT RECORD                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
LR30     CLC   KEY(RCUMMKT-RCUMKEY),SAVEKEY   COMPARE THROUGH REPCODE           
         BNE   LRX                                                              
                                                                                
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   LISTAR,SPACES                                                    
         MVC   LMARKET,RCUMMKT     PUT MARKET CODE ON SCREEN                    
*          WE NEED TO ENTER MKT CODE INTO DBLOCK TO RETRIEVE MKTNAME            
*                                                                               
         BRAS  RE,INITDBLK                                                      
         XC    DUB,DUB                                                          
         PACK  DUB,RCUMMKT(4)      CURRENTLY, 5TH CHAR IS ALWAYS A ' '          
         CVB   R0,DUB                                                           
         STCM  R0,3,DB.DBSELRMK                                                 
         BRAS  RE,CALLDEM          GET MARKET NAME                              
         MVC   LMKTNAME,MKTNAME                                                 
                                                                                
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
         B     LR20                REPEAT LOOP                                  
         DROP  R4                                                               
*                                                                               
LRX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  LHI   R0,(MISSING)                                                     
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
INVLFLD  LHI   R0,(INVALID)                                                     
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
BASEREC  LHI   R0,(837)                                                         
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
DUPLIC8  LHI   R0,(401)                                                         
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
CANNOTD  LHI   R0,(12)                                                          
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
MKTORSTA LHI   R0,(872)                                                         
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
STANDON  LHI   R0,(873)                                                         
         STCM  R0,3,RERROR                                                      
         J     ERREND                                                           
*                                                                               
                                                                                
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM97D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM98D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
         ORG   SYSSPARE                                                         
                                                                                
RECID    DS    0CL7                UNIQUE PORTION OF KEY                        
REPALPHA DS    CL2                 AGENCY CODE                                  
MKTALPHA DS    CL5                 MARKET ALPHA-NUMERIC CODE                    
*                                                                               
SAVEKEY  DS    XL27                SAVED KEY FOR LIST REDISPLAY                 
TMPBOOK  DS    XL2                 HOLDER FOR BOOK TRANSLATION                  
BLAMEFLD DS    CL10                HOLDS ACTION FOR AUDIT TRAIL                 
                                                                                
VALIDNUM EQU   X'08'               SCREEN FIELD VALIDATION FLAG                 
                                                                                
MYDBLOCK DS    XL(L'DBLOCK)                                                     
         DS    XL44                DBLOCK IS LARGER THAN IT APPEARS             
MKTINFO  DS    0CL32               COMBINED MARKET FIELDS                       
MKTNUM   DS    XL2                  MARKET NUMBER                               
MKTNAME  DS    CL30                 MARKET NAME                                 
*                                                                               
OVERENT  DS    0XL5                TEMP HOLDER FOR SINGLE OVERRIDE ENT          
OVYEAR   DS    XL1                      OVERRIDE YEAR                           
OVQTR    DS    XL1                               QUARTER                        
OVBOOK   DS    XL3                               BOOK                           
OVERTAB  DS    XL181               TABLE OF USER OVERRIDE BOOK ENTRIES          
*                                   (ALLOWS FOR MAX OF 30 + EOT )               
                                                                                
         EJECT                                                                  
*                                                                               
* ENTRY LINE DSECT                                                              
*                                                                               
ENTRY    DSECT                                                                  
ENTYEARH DS    XL8                                                              
ENTYEAR  DS    CL2                                                              
ENTQTRH  DS    XL8                                                              
ENTQTR   DS    CL1                                                              
ENTBOOKH DS    XL8                                                              
ENTBOOK  DS    CL10                                                             
ENTRYLQ  EQU   *-ENTRY                                                          
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMARKET  DS    CL5                                                              
         DS    CL8                                                              
LMKTNAME DS    CL30                                                             
*                                                                               
* OFFLINE LIST LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE REGENCUMO                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'149RESFM44   08/29/00'                                      
         END                                                                    
