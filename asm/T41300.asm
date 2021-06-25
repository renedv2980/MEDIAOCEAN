*          DATA SET T41300     AT LEVEL 120 AS OF 11/10/08                      
*PHASE T41300A                                                                  
*INCLUDE INSORPT                                                                
*INCLUDE POLPRNT                                                                
*INCLUDE WRKIO                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41300- INSERTION ORDERS BASE'                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 11/08    CORRECT REQUEST RECORD NAVIGATION                               
*                                                                               
* SMYE 03/04    GET CUREDIT ADDRESS FOR NLFMTBUY (CUSTOM COLUMNS)               
*                                                                               
* SMYE 02/03    ADD CALL FROM ADBUYER LOGIC                                     
*                                                                               
* KWAN 07/17/02 SECOND FAX                                                      
*                                                                               
* KWAN 05/17/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* BPLA 06/00    ADLIST EXPANDED - EXTEND NMOD FROM                              
*               3220 TO 3480   DOUBLE WORDS                                     
*               25760 TO 27840                                                  
*               ADLIST INCREASED BY 2010 BYTES                                  
*               TO HANDLE REPEAT DATE AN PUB                                    
*               IOWORK WAS HEX 30E8 NOW 38C2                                    
*               IOWORK WAS DEC 12520 NOW 14530                                  
*                                                                               
* BPLA 06/00    NOW LIVE SOURCE (WAS T41300B)                                   
*               PHASE, CATALP AND ++INCLUDE T413WKA RESTORED                    
*                                                                               
* BPLA 02/00    COPY OF T41300 LEVEL 103 MADE 2/8/00                            
*               INSORPB FOR INSORPT AND T413WKAB FOR T413WKA                    
*                                                                               
* BPLA 02/00    NMOD CHANGED TO ALLOW FOR LARGER IOWORK                         
*               WAS 2908 NOW 3058                                               
*                                                                               
* BPLA 12/97    DISPLAY PRNTQUE INFO                                            
*                                                                               
* SMYE 10/97    GETINS MADE CORE-RESIDENT                                       
*                                                                               
* BPLA 12/95    CHANGES FOR FRENCH I/O'S                                        
*                                                                               
* BPLA 07/13/93 NMOD1 EXPANDED TO 2980 FROM 2930                                
*               TO ACCOMDATE MORE BUYS ON AN I/O WAS 56 NOW 100                 
*               BUYDALST WAS 232 NOW 404                                        
*                                                                               
* BPLA 01/26/93 NMOD1 EXPANDED TO 2930 FROM 2870                                
*               BIGGER RECORD AREAS IN POLFILE                                  
*                                                                               
* BPLA 12/18/91 =V(REPORT) CHANGED TO =V(IREPORT) INSORPT IS NOW                
*               IREPORT                                                         
*                                                                               
* BPLA 10/18/90 FAX CHANGES (NEW INSORPT)                                       
*                                                                               
* BPLA 08/31/90 NMOD1 EXPANDED TO 2200 FROM 2128 TO ALLOW FOR                   
*               NEW POLFILE (BIGGER AREAS FOR AGY,CLT,PRD,EST                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NMOD AREA =  LENGTH OF POLWRK  + LENGTH OF IOWORK +                           
*              LENGTH OF POLFILE + LENGTH T413WKB                               
*                                                                               
*  3508 + 14530 + 2403 + 8252 = 28693                                           
*                                                                               
*  28693/8 = 3586.63 DOUBLE WORDS  (3590)                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41300   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 3590,T41300,R6,RR=R9   (NMOD SIZE IS CACULATED ABOVE)            
*                                      AND NOTE R6 AS SECOND BASE               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
         USING POLWRKD,RC                                                       
         USING T413FFD,RA                                                       
         LR    R8,RC                                                            
         A     R8,=A(POLWRKX-POLWRKD)                                           
         USING IOWORKD,R8                                                       
*                                                                               
* R4 STILL POINTS TO FAC LIST                                                   
*                                                                               
         MVC   DATCON,36(R4)       EXPANDED FACILITIES LIST                     
         MVC   VCOMFACS,16(R1)     R1 SHOULD STILL BE O.K.                      
*                                  SINCE INITL DOES NOT AFFECT IT               
         LR    R9,R8                                                            
         A     R9,=A(IOWORKX-IOWORKD)                                           
         USING POLFILED,R9                                                      
         L     RF,=V(IREPORT)                                                   
         A     RF,RELO                                                          
         ST    RF,REPORT                                                        
         L     RF,=V(PRINT)                                                     
         A     RF,RELO                                                          
         ST    RF,PRINT                                                         
         L     RF,=V(WRKIO)                                                     
         A     RF,RELO                                                          
         ST    RF,VWRKIO                                                        
*                                                                               
         L     RF,=A(FAXBUF)                                                    
         A     RF,RELO                                                          
         ST    RF,AFAXBUF                                                       
         L     RF,=A(FAXBUF2)                                                   
         A     RF,RELO                                                          
         ST    RF,AFAXBUF2                                                      
*                                                                               
         LR    RF,R8               SINCE ADLIST NOT ADDRESSABLE                 
         AHI   RF,4000                                                          
         USING IOWORKD+4000,RF                                                  
         LA    RE,ADLIST                                                        
         ST    RE,AADLIST                                                       
         DROP  RF                                                               
**********************************************************************          
******                 SET ADDRESSES                        **********          
******  FOR A 1201-BYTE TABLE TO HOLD BUY SERIAL NUMBERS    **********          
******      AND FOR WORKER REC AREAS USED BY LINK           **********          
******     (NEEDED IN CASE OF A CALL FROM ADBUYER)          **********          
**********************************************************************          
******   L     RF,VTWA            A(TWA)                                        
******   AHI   RF,6144            POINT TO END OF "OLD" TWA                     
******   ST    RF,ASERTBL         ADDRESS OF 1201 BYTE SERIAL# TABLE            
*                                                                               
*                                                                               
         L     RF,=A(SERTBL#)     POINT TO 1201 BYTE CSECT                      
         A     RF,RELO                                                          
         ST    RF,ASERTBL         ADDRESS OF 1201 BYTE SERIAL# TABLE            
*                                                                               
         L     RF,=A(WRKRECA)     POINT TO 18432 BYTE CSECT                     
         A     RF,RELO         (4096 FOR RECORD AND REST FOR BUFFER)            
         ST    RF,AWRKREC         ADDRESS FOR WORKER REC AREA                   
         AHI   RF,4096            AND                                           
         ST    RF,AWRKBUF         ADDRESS FOR WORKER REC AREA BUFFER            
**********************************************************************          
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CXSORT-COMFACSD)(RF)                                         
         ST    RF,XSORT                                                         
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGLOBBER-COMFACSD)(RF)                                       
         ST    RF,VGLOBBER                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CCUREDIT-COMFACSD)(RF)                                       
         ST    RF,VCUREDIT                                                      
*                                                                               
         MVC   FULL,=X'D9000AAB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   GETINS,DMCB         STORE GETINS ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB8'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB9'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABA'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETCG,DMCB       STORE PPGETCG ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETAD,DMCB       STORE PPGETADR ADDRESS                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         ST    R1,SAVER1    SAVE R1 TO RESTORE AFTER ADBUY CODE BELOW           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVI   ADBSW,C' '          CLEAR "CALLED BY ADBUYER" FLAG               
         BRAS  RE,CKCALLS          ANY GLOBBER CALLS ?                          
*                                                                               
         BNE   IOM0                NO - "NORMAL" PROCESSING                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*     ADBUYER CALL WORKER FILE PREPARATION AND HANDLING                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKWR     DS    0H                                                               
         MVI   ADBSW,C'Y'          INDICATE CALLED BY ADBUYER                   
         XC    WRKIOPAR(200),WRKIOPAR                                           
*                                                                               
         LA    R4,WRKIOPAR                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         MVC   WRKIAREC,AWRKREC                                                 
         MVC   WRKIABUF,AWRKBUF                                                 
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',WRKWKEY,L'WRKWKEY,GLVDLUWF                
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                CAN'T GET WORKER KEY                         
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'CLEAR'                                          
*                                                                               
         MVI   WRKIFTYP,WRKIFTWF   SET TO OPEN A WORKER FILE                    
         MVI   WRKIACTN,WRKIAOPN   OPEN THE PASSED WORKER FILE                  
         MVC   WRKIACOM,VCOMFACS                                                
*                                                                               
         GOTOR VWRKIO,WRKIOB                                                    
         JE    *+6                                                              
         DC    H'0'                CAN'T OPEN WORKER FILE                       
*                                                                               
         SR    R0,R0               R0=RECORD NUMBER                             
CKWR10   DS    0H                                                               
*                                                                               
         LA    R4,WRKIOPAR         RESET FOR WRKIOD                             
*                                                                               
         AHI   R0,1                READ FIRST/NEXT WORKER FILE RECORD           
         ICM   R7,15,WRKIAREC      R7=A(RECORD)                                 
         STCM  R0,15,0(R7)                                                      
         ST    R0,WRKRNUMB                                                      
         MVC   4(4,R7),=C'REC#'                                                 
         XC    8(4,R7),8(R7)                                                    
*                                                                               
         GOTOR VDATAMGR,WRKIPARM,(0,=C'RANDOM')                                 
         JNE   CKWR90              END OF WORKER FILE                           
*                                                                               
         DROP  R4                                                               
*                                                                               
         L     R0,ASERTBL                                                       
         LHI   R1,1201                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SERIAL NUMBER TABLE                    
*                                                                               
*                                                                               
         BRAS  RE,CLRSCR           CLEAR ALL SCREEN FIELDS                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  DETERMINE IF WORKER RECORD HAS NEEDED FIELDS (SEQUENCE SHOULD BE             
*      SCREEN ELEMENTS FOLLOWED BY ONE SERIAL NUMBER ELEMENT)                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         AHI   R7,4                POINT TO ELEM                                
         ST    R7,SVWRKELM         SAVE ELEM POINTER                            
         USING LQ_EL,R7                                                         
*                                  CHECK RECORD FOR MINIMUM CONTENT             
CKWR20   CLI   LQ_EL,0             ELEM CODES EXIST?                            
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_EL,LQ_IMAPQ      INPUT MAP NUMBER (RECORD TYPE) ELEM?         
         JE    CKWR22                                                           
         BAS   RE,NXTELEM                                                       
         J     CKWR20                                                           
*                                                                               
CKWR22   CLC   =AL2(M#ULIOR),3(R7) UPLOAD - INSERT ORDER REQUEST ?              
         JE    *+6                                                              
         DC    H'0'                MUST BE FIRST                                
CKWR25   BAS   RE,NXTELEM                                                       
         CLI   LQ_EL,0             END OF RECORD ?                              
         JNE   *+6                 NO                                           
         DC    H'0'                MUST FIND SERIAL# ELEMENTS FIRST             
         CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ?                       
         JNE   *+6                 WOULD BE EOR - SHOULD NOT GET HERE           
         DC    H'0'                 WITHOUT FINDING SERIAL# ELEMENT             
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEMENT ?                       
         JE    CKWR25C             YES - SEE IF SERIAL# ELEMENT                 
         DC    H'0'                NO - NOT GOOD                                
CKWR25C  DS    0H                                                               
         CLC   =AL2(D#BYSER),3(R7)    SERIAL NUMBER MAP CODE ?                  
         JNE   CKWR25              NO - CONTINUE LOOK FOR SERIAL#S              
*                                  SERIAL# ELEMENT FOUND                        
         L     R7,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
**********************************************************************          
*                                  PROCESS THE WORKER RECORD                    
**********************************************************************          
CKWR28   CLI   LQ_EL,0             END OF RECORD ?                              
         JNE   *+6                                                              
         DC    H'0'                SOMETHING REALLY WRONG                       
         CLI   LQ_EL,LQ_IMAPQ      INPUT MAP NUMBER ELEMENT ?                   
         JE    CKWR28F                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEMENT ?                       
         JE    CKWR28E                                                          
         BAS   RE,NXTELEM                                                       
         J     CKWR28                                                           
*                                                                               
CKWR28E  CLC   =AL2(D#BYSER),3(R7)    SERIAL NUMBER MAP CODE ?                  
         JE    CKWR30      GO BUILD SERIAL# TABLE AND FINISH THIS REC           
*                                                                               
*                                  IF NOT SERIAL NUMBER MAP CODE                
*                                  MUST BE A SCREEN FIELD MAP CODE SO..         
         BRAS  RE,FILSCR           ANALYZE AND PUT FIELD TO SCREEN              
CKWR28F  BAS   RE,NXTELEM                                                       
         J     CKWR28                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BUILD TABLE OF SERIAL NUMBERS OF BUYS TO BE INCLUDED IN INSERT ORDER          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          R7 POINTING TO SERIAL# ELEMENT - SHOULD BE           
CKWR30   DS    0H          ONE ONLY ELEMENT CONTAINING 1-NN SERIAL #S           
         CLI   5(R7),LQ_TSINQ     ONE VALUE ?                                   
         BNE   CKWR32             NO - MUST BE LIST OF VALUES                   
         L     R4,ASERTBL         POINT TO START OF SERIAL# TABLE               
         PACK  DUB,6(9,R7)        PACK EBCDIC DATA FROM ELEMENT                 
         MVC   1(5,R4),DUB+3      MOVE TO SERIAL # TABLE                        
         MVI   6(R4),X'FF'        END OF SERIAL NUMBER TABLE                    
         B     CKWR38             DONE WITH SERIAL#                             
*                                                                               
CKWR32   CLI   5(R7),LQ_TLSTQ     LIST OF VALUES ?                              
         BE    *+6                                                              
         DC    H'0'               CAN'T BE OTHER THAN ONE VALUE OR LIST         
         OC    6(2,R7),6(R7)      SERIAL# COUNTER POSITIVE ?                    
         BNZ   *+6                                                              
         DC    H'0'               NO                                            
         ZICM  R0,6(R7),2         SERIAL# COUNT TO LOOP COUNTER                 
         CHI   R0,201                                                           
         BL    *+6                                                              
         DC    H'0'               MAXIMUM OF 200 EXCEEDED                       
         LR    RE,R7                                                            
         LA    RE,8(RE)           POINT TO START OF SERIAL# "STRING"            
         L     R4,ASERTBL         POINT TO START OF SERIAL# TABLE               
CKWR35   PACK  DUB,0(9,RE)        PACK EBCDIC DATA FROM "STRING"                
         MVC   1(5,R4),DUB+3      MOVE TO SERIAL # TABLE                        
         LA    R4,6(R4)           BUMP TO NEXT SLOT IN SER# TBL                 
         LA    RE,9(RE)           BUMP TO NEXT SERIAL# AREA IN ELEM             
         BCT   R0,CKWR35                                                        
         MVI   0(R4),X'FF'        END OF SERIAL NUMBER TABLE                    
*                                                                               
CKWR38   BAS   RE,NXTELEM       MUST BE AT EOR OR A RETURN DATA HEADER          
         CLI   LQ_EL,0            END OF RECORD ?                               
         JE    CKWR40             OK                                            
         CLI   LQ_EL,LQ_RDATQ     RETURNED DATA HEADER ?                        
         JE    *+6                OK                                            
         DC    H'0'               SHOULD NOT GET HERE                           
*                                                                               
CKWR40   DS    0H                 CHECK THAT MINIMUM SCREEN CONTENT             
*                                 WAS OBTAINED FROM WORKER RECORD               
         CLC   PRQUID,=3C' '      REPORT ID ?                                   
         BNH   CKWR40NG           NO                                            
         CLI   PRQMED,C' '        MEDIA CODE ?                                  
         BNH   CKWR40NG           NO                                            
         CLI   PRQCLT,C' '        CLIENT CODE ?                                 
         BNH   CKWR40NG           NO                                            
         CLI   PRQPRD,C' '        PRODUCT CODE ?                                
         BNH   CKWR40NG           NO                                            
         CLI   PRQPUB,C' '        PUB CODE ?                                    
         BNH   CKWR40NG           NO                                            
         CLI   PRQPER,C' '        START-END DATES ?                             
         BH    IOM0               YES -OK - OFF TO EDITS, ETC.                  
CKWR40NG DC    H'0'               REQUIRED SCREEN FIELDS MISSING                
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*         END OF WORKER FILE. CLOSE WORKER FILE                                 
*         AND RETURN TO CALLER VIA GLOBBER XCTL                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKWR90   DS    0H                 END OF WORKER FILE                            
*                                                                               
         LA    R4,WRKIOPAR                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         MVI   ADBSW,C' '         CLEAR CALLED BY ADBUYER FLAG                  
         MVI   WRKIACTN,WRKIACLO  CLOSE WORKER FILE                             
         GOTOR VWRKIO,WRKIOB                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
****************************************************************                
         NOP   EXXMODX        FOR TESTING ONLY                                  
****************************************************************                
*                                                                               
         DROP  R4                                                               
*                                                                               
* * * *  RETURN TO CALLER VIA GLOBBER XCTL  * * * *                             
*                                                                               
         LA    R1,GLOBWORK                                                      
         USING GLVXFRSY,R1                                                      
         MVI   GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         DROP  R1                                                               
         GOTOR VGLOBBER,DMCB,=C'PUTD',GLOBWORK,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,0                                                         
         JE    EXXMODX                                                          
         DC    H'0'                GLOBBER ERROR ENCOUNTERED                    
*                                                                               
**********************************************************************          
*                                                                               
NXTELEM  ZICM  RF,LQ_LN,(3)                                                     
         AR    R7,RF                                                            
         BR    RE                                                               
*                                                                               
**********************************************************************          
*************   ADBUYER INPUT/OUTPUT WORK ABOVE    *******************          
**********************************************************************          
*                                                                               
*                                                                               
IOM0     DS    0H                                                               
*                                                                               
         L     R1,SAVER1    RESTORE TO ADDRESS PRIOR TO ADBUYER WORK            
*                                                                               
         XC    SVERRFLD,SVERRFLD   CLEAR ERROR FIELD NUMBER (FOR ADB)           
*                                                                               
         MVI   ROUTE,1             HEADLINE EDIT                                
         BAS   RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         CLI   NEWKSW,C'Y'         TEST KEY FIELD CHANGED                       
         BE    IOM2                YES - START FRESH                            
*                                                                               
IOM1     MVC   KEY,SVLSTKEY        RESTART WITH LAST KEY                        
         XC    SVLSTKEY,SVLSTKEY                                                
         OC    KEY,KEY                                                          
         BZ    IOM2                                                             
*                                                                               
         BAS   RE,HIGH                                                          
         B     IOM3                                                             
*                                                                               
IOM2     XC    KEY,KEY                                                          
         XC    SCPSCOM1,SCPSCOM1   FIRST TIME - CLEAR SAVED                     
         XC    SCPSCOM2,SCPSCOM2   STANDARD COMMENTS                            
         XC    CPSCOM1,CPSCOM1                                                  
         XC    CPSCOM2,CPSCOM2                                                  
         MVI   LANG,0                                                           
         MVI   SVLANG,0                                                         
*                                                                               
IOM3     L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R4,DMCB             ADDR OF GETFACT'S WORKING STORAGE            
         USING FACTSD,R4                                                        
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO                                                    
         M     R2,=F'90'           USE 90% OF MAX                               
         D     R2,=F'100'                                                       
*                                                                               
***************  TEMPORARY FOR TESTING  ****************                        
*TEST*   LA    R3,2                                                             
***************  TEMPORARY FOR TESTING  ****************                        
*                                                                               
         STH   R3,SVMAXIO          USE 90% OF MAX                               
*                                                                               
         B     IOM3X               SKIP FOR NOW                                 
*                                                                               
         CLC   FALINE,=C'DFLA'     FOR LINE ID'S DFLA AND LFLA                  
         BE    IOM3W               SEND TO CLASS W                              
         CLC   FALINE,=C'LFLA'                                                  
         BE    IOM3W                                                            
         B     IOM3X                                                            
*                                                                               
IOM3W    L     RE,VTIA                                                          
         LA    RE,4(RE)                                                         
         USING PQPLD,RE                                                         
         MVI   QLCLASS,C'W'                                                     
         DROP  RE                                                               
*                                                                               
IOM3X    DS    0H                                                               
         DROP  RF                                                               
         DROP  R4                                                               
*                                                                               
IOM4     DS    0H                                                               
*                                                                               
         XC    SVERRFLD,SVERRFLD   CLEAR ERROR FIELD NUMBER (FOR ADB)           
*                                                                               
         MVI   ROUTE,2                                                          
         BAS   RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         CLI   ERRAREA,0           ON ERROR SET END OF FILE                     
         BE    IOM4G                                                            
         CLI   ERRAREA,X'FE'       REC LOCK ERROR?                              
         BNE   IOM4D                                                            
         MVI   ERRAREA,0                                                        
         LA    R2,PRQUIDH          POINT TO REQUESTOR ID FIELD                  
         LA    R3,DATALOCK         DATA LOCKED ERR MSG                          
         B     ERROR                                                            
*                                                                               
IOM4D    MVI   PBUYKEY,X'FF'                                                    
*                                  LAST BUY MUST SET LAST COMMENTS              
IOM4G    CLI   PBUYKEY,X'FF'                                                    
         BNE   IOM4J                                                            
         MVC   SCPSCOM1,CPSCOM1                                                 
         MVC   SCPSCOM2,CPSCOM2                                                 
         MVC   SVLANG,LANG                                                      
*                                                                               
IOM4J    MVI   ROUTE,X'10'                                                      
         BAS   RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         LA    R4,WORK                                                          
         L     R1,0(RF)            A(*) IN T41310                               
         LA    RF,4(RF)                                                         
IOM5     DS    0H                                                               
         L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1               MUST SUBTRACT A(*) IN T41310                 
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   IOM5                                                             
*                                                                               
         MVC   APINSOR(08),WORK          RELOCATE ACONS                         
*                                                                               
         L     RF,=A(LOAD12)       FMTBUY                                       
         A     RF,RELO                                                          
         ST    RF,AFMTBUY                                                       
*                                                                               
         L     RF,=A(LOAD11)       LOADER FOR OTHER ROUTINES                    
         A     RF,RELO                                                          
         ST    RF,ASTDCOM                                                       
         ST    RF,ACOMLIN                                                       
         ST    RF,AMATCOM                                                       
         ST    RF,ABLDREV                                                       
         MVI   ACOMLIN,4           SET BRANCH DISP                              
         MVI   AMATCOM,8                                                        
*                                                                               
         MVI   ABLDREV,12                                                       
         LA    RF,WFMOD            SET DUMMY WFMOD AND ERRMOD                   
         ST    RF,AWFMOD                                                        
         ST    RF,AERRMOD                                                       
         B     *+6                                                              
WFMOD    BR    RE                                                               
*                                                                               
         LA    RF,RECPOOL                                                       
         ST    RF,ARECPOOL                                                      
*                                                                               
         LR    RF,R8               TO ADDRESS COMTAB AND BUYMAX                 
         AHI   RF,4000                                                          
         USING IOWORKD+4000,RF                                                  
         LA    RE,COMTAB                                                        
         ST    RE,ACOMTAB                                                       
         LA    RE,BUYMAX                                                        
         ST    RE,ABUYMAX                                                       
         DROP  RF                                                               
*                                                                               
         L     R5,BUYDALST                                                      
         AHI   R5,-4                                                            
         L     R5,0(R5)            SAVE DA OF LAST BUY                          
*                                                                               
         GOTO1 APINSOR,DMCB,(RC)                                                
*                                                                               
         CLI   ERRAREA,0                                                        
*ADB*    BNE   EXXMOD                                                           
         BNE   EXXMODX                                                          
         OC    ERR(2),ERR                                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    IOM7                                                             
         L     R4,BUYDALST                                                      
         AHI   R4,-4                                                            
         C     R5,0(R4)            TEST DA OF LAST BUY                          
         BE    IOM7                                                             
*                                                                               
         L     RF,INSCNT           COUNT OF INSERTIONS                          
         LA    RF,1(RF)                                                         
         ST    RF,INSCNT                                                        
*                                                                               
IOM7     DS    0H                                                               
         CLC   REFNO,PAGYIONO                                                   
         BE    IOM8                                                             
         L     RF,IOCNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,IOCNT                                                         
         MVC   PAGYIONO,REFNO                                                   
*                                                                               
         CLI   PBUYKEY,X'FF'                                                    
         BE    IOM8C               SEE IF DONE                                  
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,0,0,0                                              
         L     R4,DMCB             USE GETFACT'S RETURNED WORKAREA              
         USING FACTSD,R4                                                        
*                                                                               
*                                                                               
         CLC   FATIOCNT,SVMAXIO    TEST REACHED LIMIT                           
         BL    IOM8                NO - CONTINUE                                
*                                                                               
* SETS PBUYREC TO X'FF'                                                         
*                                                                               
IOM7H    MVC   SVLSTKEY,KEY                                                     
         B     IOM4D               MUST GO FINISH THIS I/O                      
*                                                                               
         DROP  R4                                                               
         DROP  RF                                                               
*                                                                               
IOM8     DS    0H                                                               
         CLI   PBUYKEY,X'FF'                                                    
         BNE   IOM4                GET NEXT BUY                                 
*                                                                               
* END OF BUYS                                                                   
*                                                                               
IOM8C    EDIT  INSCNT,(4,PRQLN1)                                                
         OI    PRQLN1+3,X'F0'                                                   
*                                                                               
         MVC   PRQLN1+5(10),=C'INSERTIONS'                                      
         C     R0,=F'1'                                                         
         BNE   *+8                                                              
         MVI   PRQLN1+14,C' '                                                   
         EDIT  IOCNT,(4,PRQLN2)                                                 
         OI    PRQLN2+3,X'F0'                                                   
*                                                                               
         MVC   PRQLN2+5(11),=C'INS. ORDERS'                                     
         C     R0,=F'1'                                                         
         BNE   *+8                                                              
         MVI   PRQLN2+15,C' '                                                   
         FOUT  PRQLN1H                                                          
         FOUT  PRQLN2H                                                          
*                                                                               
         XC    PRQLN3,PRQLN3                                                    
         LTR   R0,R0                                                            
         BZ    IOM9                NO IO'S                                      
*                                                                               
         MVC   PRQLN3+5(12),=C'REPORT NAME='                                    
         MVC   PRQLN3+18(8),RCJOB                                               
         L     RF,VTIA                                                          
         CLC   0(5,RF),=C'START'                                                
         BE    IOM9                                                             
         USING UKRECD,RF                                                        
         MVC   PRQLN3+28(3),UKSUBID                                             
         MVI   PRQLN3+31,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+32),ALIGN=LEFT                            
         CLI   QOPT7,C'F'      SEE IF FAXING                                    
         BNE   IOM9                                                             
*                                                                               
         L     RF,AFAXBUF                                                       
         MVC   PRQLN3+37(3),UKSUBID                                             
         MVI   PRQLN3+40,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+41),ALIGN=LEFT                            
*                                                                               
         OC    SECONDFX,SECONDFX   SECOND FAX PRESENT?                          
         BZ    IOM9                                                             
         L     RF,AFAXBUF2                                                      
         MVC   PRQLN3+46(3),UKSUBID                                             
         MVI   PRQLN3+49,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+50),ALIGN=LEFT                            
*                                                                               
         DROP   RF                                                              
*                                                                               
IOM9     DS    0H                                                               
         FOUT  PRQLN3H             END PRINTING                                 
*                                                                               
         L     RF,VTIA                                                          
         CLC   0(5,RF),=C'START'                                                
         BE    IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,VTIA,VDATAMGR                               
*                                                                               
         CLI   QOPT7,C'F'                                                       
         BNE   IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,AFAXBUF,VDATAMGR                            
*                                                                               
         OC    SECONDFX,SECONDFX   SECOND FAX PRESENT?                          
         BZ    IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,AFAXBUF2,VDATAMGR                           
*                                                                               
IOM10    DS    0H                                                               
         CLI   RCWRITE,C'N'        SEE IF 'TEST' RUN                            
         BE    IOM13               YES - DON'T WRITE BACK AGENCY HEADER         
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY+27(4),AGYDA                                                  
         BAS   RE,GETPRT                                                        
         MVC   PAGYIONO,REFNO                                                   
         MVC   PAGYIODT,BTODAY                                                  
         BAS   RE,PUTPRT                                                        
*                                                                               
         CLI   ERRAREA,0                                                        
*ADB*    BNE   EXXMOD                                                           
         BNE   EXXMODX                                                          
IOM13    OC    SVLSTKEY,SVLSTKEY   SEE IF CONTINUATION PENDING                  
         BZ    IOM15                                                            
         XC    PRQMSG,PRQMSG                                                    
         MVC   PRQMSG(L'CONMSG),CONMSG                                          
         OI    PRQUIDH+1,X'01'     MODIFY REQ ID FOR NEXT INPUT                 
         FOUT  PRQUIDH                                                          
         B     IOM17                                                            
*                                                                               
IOM15    MVC   PRQMSG(L'OKMSG),OKMSG                                            
IOM17    FOUT  PRQMSGH                                                          
*                                                                               
         LA    R2,PRQMEDH                                                       
         MVI   DATALKSW,0          RESET DATA LOCKED SWITCH                     
         BRAS  RE,UNLOCK           UNLOCK KEY SET IN THIS APPLICATION           
*                                                                               
*                                                                               
         CLI   ADBSW,C'Y'          PROCESSING AN ADBUYER CALL ?                 
         JNE   EXIT                NO                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        WE HAVE FINISHED ONE ADBUYER INSERTION ORDER SO WRITE                  
*        A RETURN RECORD HERE FOR ADBUYER AND                                   
*        THEN GO READ FOR ANOTHER WORKER FILE RECORD                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IOM60    DS    0H                                                               
         L     R7,SVWRKELM        POINT TO BEGINNING OF WRKR REC                
         USING LQ_EL,R7                                                         
IOM60D   BAS   RE,NXTELEM                                                       
         CLI   LQ_EL,0                                                          
         BNE   *+6                                                              
         DC    H'0'               SHOULD NOT HAPPEN                             
         CLI   LQ_EL,LQ_RDATQ     RETURNED HEADER ELEMENT (LAST)                
         BNE   IOM60D                                                           
*                R7 NOW POINTING TO 09 ELEM ENDING THE INPUT RECORD             
*                * * * * * PUT ALL NEW OUTPUT ELEMENTS HERE * * * *             
         MVI   LQ_EL,LQ_DLDDQ     DOWNLOAD DATA ELEMENT (MUST BE 1ST)           
         LHI   RE,5               ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         MVC   3(2,R7),=X'0121'   RECORD CODE FOR "NORMAL" REPLY                
*                                                                               
         OC    SVLSTKEY,SVLSTKEY  SEE IF CONTINUATION PENDING                   
         BZ    IOM60F             NO - OK                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   "PARTIAL" COMPLETION OF I/O REQUEST BECAUSE MAXIO WAS REACHED               
*      SEND MESSAGE ELEMENT ALONG WITH "NORMAL" RETURN FIELDS                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         XC    SVLSTKEY,SVLSTKEY  CLEAR FOR NEXT REQUEST                        
*                                                                               
         AR    R7,RE              NEXT ELEMENT                                  
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,66              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#ERRDSC        ERROR DESCRIPTION MAP CODE                    
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(L'MORMSG,R7),MORMSG    NOT ALL BUYS PROCESSED (MAXIO)          
*                                                                               
IOM60F   AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,10              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#INS           MAP CODE FOR # INSERTIONS PROCESSED           
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(4,R7),PRQLN1     NUMBER OF INSERTIONS HERE                     
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,10              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#IOS           MAP CODE FOR # INSERTION ORDERS               
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(4,R7),PRQLN2     NUMBER OF INSERTION ORDERS HERE               
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         CLI   PRQLN3+18,C' '     ANYTHING IN REPORT NAME ?                     
         BNH   IOM60P             NO - SKIP TO SERIAL#S ELEMENT                 
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,14              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#RPTNM         MAP CODE FOR REPORT NAME                      
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(8,R7),PRQLN3+18  REPORT NAME HERE                              
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         CLI   PRQLN3+28,C' '     ANYTHING IN REPORT ID ?                       
         BNH   IOM60P             NO - SKIP TO SERIAL#S ELEMENT                 
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,14              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#RPTID         MAP CODE FOR REPORT ID                        
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(8,R7),PRQLN3+28  REPORT ID HERE                                
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         CLI   PRQLN3+37,C' '     ANYTHING IN FAX1 ID ?                         
         BNH   IOM60P             NO - SKIP TO SERIAL#S ELEMENT                 
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,14              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#FX1ID         MAP CODE FOR FAX1 ID                          
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(8,R7),PRQLN3+37  FAX1 ID                                       
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
         CLI   PRQLN3+46,C' '     ANYTHING IN FAX2 ID ?                         
         BNH   IOM60P             NO - SKIP TO SERIAL#S ELEMENT                 
*                                                                               
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,14              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#FX2ID         MAP CODE FOR FAX2 ID                          
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(8,R7),PRQLN3+46  FAX2 ID                                       
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
IOM60P   DS    0H                 OUTPUT THE SERIAL #S FOR ANY BUYS             
*                                 NOT INCLUDED IN INSERTION ORDER(S)            
         L     R4,ASERTBL         POINT TO SERIAL# TABLE                        
IOM60R   CLI   0(R4),X'FF'        END OF TABLE ?                                
         BE    IOM60X             YES                                           
         OC    1(5,R4),1(R4)                                                    
         BNZ   *+6                                                              
         DC    H'0'               MUST HAVE SERIAL #                            
         CLI   0(R4),C'Y'         SERIAL # "USED" ?                             
         BE    IOM60U             YES - NEXT TABLE ENTRY                        
         UNPK  WORK(9),1(5,R4)    NO - CONVERT TO EBCDIC                        
         OI    WORK+8,X'F0'       AND                                           
*                                 PUT INTO A . .                                
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,15              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#BYSER         MAP CODE FOR BUY SERIAL NUMBER                
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         MVC   6(9,R7),WORK       EBCDIC SERIAL NUMBER                          
         AR    R7,RE              NEXT ELEMENT                                  
*                                                                               
IOM60U   LA    R4,6(R4)           BUMP TO NEXT ENTRY                            
         B     IOM60R                                                           
*                                                                               
IOM60X   DS    0H                                                               
*                                                                               
*                                                                               
IOM80    DS    0H                                                               
*                                                                               
         LA    R4,WRKIOPAR        RESET FOR WRKIOD                              
         USING WRKIOD,R4                                                        
*                                                                               
         MVI   WRKIACTN,WRKIAPUT                                                
         GOTOR VWRKIO,WRKIOB      WRITE BACK "UPDATED" RECORD                   
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVI   ERRAREA,0          RESET FOR NEXT RECORD                         
*                                                                               
         XC    INSCNT,INSCNT      CLEAR FOR POSSIBLY ANOTHER                    
         XC    IOCNT,IOCNT          INSERTION ORDER REQUEST                     
         L     R0,WRKRNUMB                                                      
         J     CKWR10             GO GET NEXT UPLOAD RECORD                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   ABOVE ENDS WRITING OF WORKER FILE REPLY RECORD FOR ADBUYER                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
GETOVLY  NTR1                                                                   
         SPACE 2                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*ADB*    B     EXXMOD                                                           
         B     EXXMODX                                                          
*                                                                               
         SPACE 3                                                                
OKMSG    DC    C'REQUEST PROCESSED - ENTER NEXT REQUEST'                        
CONMSG   DC    C'HIT ENTER TO CONTINUE PROCESSING'                              
MORMSG   DC    CL60'INCOMPLETE - BUYS NOT INCLUDED ARE FLAGGED'                 
         SPACE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        ADBUYER TO INSERTION ORDER STORAGE AND SAVE AREAS                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WRKIOPAR DS    50F                 200 BYTES FOR WRKIO PARAMETERS               
*                                                                               
SVWRKELM DS    F                   SAVE POINTER TO WORKER ELEM                  
WRKRNUMB DS    F                   WORKER REC NUMBER (FOR RANDOM READS)         
WRKRCCNT DS    F                   NUMB OF WORKER REC PROCESSED COUNTER         
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLINITL1                                                      
*                                                                               
       ++INCLUDE POLDMGR                                                        
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    DS    0H                                                               
         L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   DS    0H                                                               
*                                                                               
         CLI   ADBSW,C'Y'         DOING ADBUYER CALL ?                          
         BNE   EXXMODX            NO                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        WRITE ERROR REPLY RECORD HERE FOR ERROR IN ADBUYER CALL                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         OC    SVERRFLD,SVERRFLD  ERROR IN PHASE 1 OR 2 ?                       
         BNZ   EXXMOD4            YES                                           
*                                 MUST HAVE A MAP FIELD FOR ERROR               
         LHI   R0,D#REQID         USE REQUEST (REPORT) ID AS MAP FIELD          
         STH   R0,SVERRFLD                                                      
*                                                                               
EXXMOD4  DS    0H                                                               
*                                                                               
         CLI   PRQMSG,C' '        ANYTHING IN MESSAGE ?                         
         BH    *+6                YES                                           
         DC    H'0'               SHOULD NOT HAPPEN                             
*                                                                               
         L     R7,SVWRKELM        POINT TO BEGINNING OF WRKR REC                
         USING LQ_EL,R7                                                         
EXXMOD8  BAS   RE,NXTELEM                                                       
         CLI   LQ_EL,0                                                          
         BNE   *+6                                                              
         DC    H'0'               SHOULD NOT HAPPEN                             
         CLI   LQ_EL,LQ_RDATQ     RETURNED HEADER ELEMENT (LAST)                
         BNE   EXXMOD8                                                          
*                R7 NOW POINTING TO 09 ELEM ENDING THE INPUT RECORD             
*                * * * * * PUT ALL NEW OUTPUT ELEMENTS HERE * * * *             
         MVI   LQ_EL,LQ_DLDDQ     DOWNLOAD DATA ELEMENT (MUST BE 1ST)           
         LHI   RE,5               ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         MVC   3(2,R7),=X'0122'   RECORD CODE (ERROR REPLY)                     
*                                                                               
         AR    R7,RE              NEXT ELEMENT                                  
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,8               ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#ERRNUM        ERROR NUMBER MAP CODE                         
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_UBINQ     BINARY DATA TYPE INDICATOR                    
         MVC   6(2,R7),SVERRFLD   MAP CODE OF FIELD IN ERROR                    
*                                                                               
         AR    R7,RE              NEXT ELEMENT                                  
         MVI   LQ_EL,LQ_RAWDQ     RAW DATA DOWNLOAD ELEMENT                     
         LHI   RE,66              ELEMENT LENGTH                                
         STH   RE,1(R7)                                                         
         LHI   R0,D#ERRDSC        ERROR DESCRIPTION MAP CODE                    
         STH   R0,3(R7)                                                         
         MVI   5(R7),LD_CHARQ     CHARACTER DATA TYPE INDICATOR                 
         OC    PRQMSG,SPACES                                                    
         MVC   6(L'PRQMSG,R7),PRQMSG    ERROR MESSAGE                           
*                                                                               
         J     IOM80              GO WRITE RECORD AND CONTINUE                  
*                                                                               
*                                                                               
EXXMODX  XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETCORES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),FULL      CORE-RESIDENT PHASE TO BE CALLED             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                       RETURN ADDRESS IN DMCB                      
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*             CHECK FOR CALL FROM LINK TO INS                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCALLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    CKCALLER            RETURN NO CALLS                              
*                                                                               
         XC    GLOBWORK,GLOBWORK                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',GLOBWORK,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF                                                    
         BE    CKCALLER            RETURN NO CALLS                              
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GLOBWORK(12),=C'PRILINPRIINS'                                    
         BNE   CKCALLER           NOT A CALL FROM LINK TO INS                   
*                                                                               
* DELETE 'OLD' TRANSFER ELEM                                                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CKCALLX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKCALLER LTR   RB,RB               NOT EQUAL (NO CALLS FROM DDLINK)             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*     * * * *  CLEAR ALL UNPROTECTED FIELDS ON SCREEN  * * * *                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PRQUIDH          POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
*                                                                               
CSCRTOP  DS    0H                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    CSCRBMP                                                          
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    CSCREND             DONE IF SCREEN END REACHED                   
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
*                                                                               
CSCRBMP  DS    0H                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     CSCRTOP                                                          
*                                                                               
CSCREND  DS    0H                                                               
*                                                                               
CLRSCRX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*    FILL IN FIELDS ON SCREEN FROM LINK RECORD (FROM ADBUYER)                   
*                                                                               
*          R7 POINTING TO A REQUEST DATA ELEMENT                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* FILL IN PASSED FLDS, MEDIA, CLT, PRD, PUB, DATES, ETC.                        
*                                                                               
         CLC   =AL2(D#REQID),3(R7)   REPORT ID (INPUT INITIALS) ?               
         BE    FSCREQ                                                           
         CLC   =AL2(D#MEDCOD),3(R7)  MEDIA CODE ?                               
         BE    FSCMED                                                           
         CLC   =AL2(D#CLTCOD),3(R7)  CLIENT CODE ?                              
         BE    FSCCLT                                                           
         CLC   =AL2(D#PRDCOD),3(R7)  PRODUCT CODE ?                             
         BE    FSCPRD                                                           
         CLC   =AL2(D#ESTNUM),3(R7)  ESTIMATE NUMBER ?                          
         BE    FSCEST                                                           
         CLC   =AL2(D#ADCODE),3(R7)  AD CODE ?                                  
         BE    FSCADC                                                           
         CLC   =AL2(D#PUBCOD),3(R7)  PUB CODE ?                                 
         BE    FSCPUB                                                           
         CLC   =AL2(D#STEND),3(R7)   START-END DATES ?                          
         BE    FSCDATES                                                         
         CLC   =AL2(D#CTDTE),3(R7)   CONTROL DATE ?                             
         BE    FSCCDATE                                                         
         CLC   =AL2(D#TSTYN),3(R7)   TEST RUN ?                                 
         BE    FSCTST                                                           
         CLC   =AL2(D#NDOYN),3(R7)   NEEDED ONLY ?                              
         BE    FSCNEED                                                          
         CLC   =AL2(D#FAX#2),3(R7)   ADDITIONAL FAX TO: ?                       
         BE    FSCAFAX                                                          
         CLC   =AL2(D#SPCST),3(R7)   SUPPRESS COST ON ADDITIONAL COPY ?         
         BE    FSCSUPC                                                          
         CLC   =AL2(D#INSCO1),3(R7)  COMMENT # 1                                
         BE    FSCCOM1                                                          
         CLC   =AL2(D#INSCO2),3(R7)  COMMENT # 2                                
         BE    FSCCOM2                                                          
         CLC   =AL2(D#INSCO3),3(R7)  COMMENT # 3                                
         BE    FSCCOM3                                                          
         DC    H'0'                  MUST BE ONE OF ABOVE                       
*                                                                               
FSCREQ   DS    0H                 REPORT (REQUEST) ID                           
         LA    R4,PRQUIDH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCMED   DS    0H                 MEDIA CODE                                    
         LA    R4,PRQMEDH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCCLT   DS    0H                 CLIENT CODE                                   
         LA    R4,PRQCLTH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCPRD   DS    0H                 PRODUCT CODE                                  
         LA    R4,PRQPRDH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCEST   DS    0H                 ESTIMATE NUMBER                               
         LA    R4,PRQESTH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         OI    4(R4),X'0A'        SET AS VALID NUMERIC AND HEX                  
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCADC   DS    0H                 AD (JOB) CODE                                 
         LA    R4,PRQJOBH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCPUB   DS    0H                 PUBLICATION CODE                              
         LA    R4,PRQPUBH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         OI    4(R4),X'0A'        SET AS VALID NUMERIC AND HEX                  
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCDATES DS    0H                 START - END DATES                             
         LA    R4,PRQPERH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCCDATE DS    0H                 CONTROL DATE                                  
         LA    R4,PRQCDTH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCTST   DS    0H                 TEST RUN                                      
         LA    R4,PRQRUNH         POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCNEED  DS    0H                 NEEDED ONLY                                   
         LA    R4,PRQNEEDH        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCAFAX  DS    0H                 ADDITIONAL FAX TO                             
         LA    R4,PRQFAX2H        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCSUPC  DS    0H                 SUPPRESS COST ON ADDITIONAL COPY              
         LA    R4,PRQSCOSH        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCCOM1  DS    0H                 COMMENT # 1                                   
         LA    R4,PRQCOM1H        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCCOM2  DS    0H                 COMMENT # 2                                   
         LA    R4,PRQCOM2H        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FSCCOM3  DS    0H                 COMMENT # 3                                   
         LA    R4,PRQCOM3H        POINT TO SCREEN HEADER                        
         BAS   RE,FSCMOVE         MOVE DATA TO SCREEN FIELD                     
         B     FILSCRX            DONE WITH THIS ELEMENT                        
*                                                                               
FILSCRX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
FSCMOVE  NTR1                     ***** OUTPUT SCREEN FIELD                     
*                                 R7=WORKER REC ELEMENT                         
*                                 R4=SCREEN HEADER                              
         ZICM  R3,1(R7),2         LENGTH OF ELEMENT                             
         AHI   R3,-7              ELEM OVERHEAD(6)+1 FOR EX MOVE                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),6(R7)      MOVE ELEMENT DATA TO SCREEN                   
         LA    R3,1(R3)           BUMP TO ACTUAL DATA LENGTH                    
         STC   R3,5(R4)           SET SCREEN FIELD HDR LNTH                     
         MVI   4(R4),0            KILL INPUT INDICATORS                         
         OI    1(R4),X'01'        SET MODIFIED                                  
         OI    6(R4),X'80'        TRANSMIT                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* THIS ROUTINE UNLOCKS LOCKS PUT OUT EARLIER                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UNLOCK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PRQMED                                                 
         MVC   L.LOCKCLT,PRQCLT                                                 
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         OC    BPUB(4),BPUB        NOTHING IN BASE PUB NUMBER?                  
         BZ    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         MVC   L.LOCKRTY,=C'BP'    PUB LOCK                                     
         MVC   L.LOCKPUB,BPUB                                                   
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
UNLK2    L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),VCOMFACS                           
*                                                                               
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    UNLK2                                                            
*                                                                               
UNLKEQ   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
UNLKNEQ  LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
         DROP  L                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLKWKA DS    0H                                                               
*                                                                               
DATALOCK EQU   158                 DATA LOCKED ERROR MSG                        
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
*                                                                               
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                                                              
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOAD12   CSECT                                                                  
         NMOD1 0,LOAD12                                                         
         LR    RC,R8               R8 POINTS TO IOWORK                          
         S     RC,=A(POLWRKX-POLWRKD)                                           
         USING POLWRKD,RC                                                       
         CLI   LASTPH,X'12'                                                     
         BE    LD12E                                                            
         MVI   ROUTE,X'12'                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LASTPH,X'12'                                                     
*                                                                               
LD12B    L     RF,DMCB                                                          
         L     R1,0(RF)            A(*) IN T41311                               
         LA    RF,4(RF)                                                         
         L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1                                                            
         ST    R0,RFMTBUY        SAVE REAL ADDRESS                              
         LA    RF,4(RF)            GET PAST FMTBUY                              
         LA    R4,APPBYOUT         RELOCATE OTHERS                              
LD12D    L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1                                                            
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   LD12D                                                            
*                                                                               
LD12E    GOTO1 RFMTBUY         GOT REAL FMTBUY                                  
*                                                                               
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOAD11   CSECT                                                                  
         NMOD1 0,LOAD11                                                         
         LR    RC,R8               R8 POINTS TO IOWORK                          
         S     RC,=A(POLWRKX-POLWRKD)                                           
         USING POLWRKD,RC                                                       
         ST    RF,FULL          HIGH ORDER BYTE HAS DISP                        
         MVC   DMCB+16(8),DMCB           SAVE PARAMETERS                        
         CLI   LASTPH,X'11'                                                     
         BE    LD11E                                                            
         XC    DMCB(12),DMCB                                                    
         MVI   ROUTE,X'11'                                                      
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LASTPH,X'11'                                                     
*                                                                               
         LA    R4,RSTDCOM         SAVE REAL ADDRESSES                           
         L     RF,DMCB                                                          
         L     R1,0(RF)            A(*) IN T41311                               
         LA    RF,4(RF)                                                         
LD11B    L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1               MUST SUBTRACT A(*) IN T41311                 
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   LD11B                                                            
*                                                                               
LD11E    LA    R4,RSTDCOM             BRANCH TO REAL ROUTINE                    
         ZIC   R2,FULL             GET DISP                                     
         LA    R4,0(R2,R4)                                                      
         L     RF,0(R4)                                                         
         MVC   DMCB(8),DMCB+16       RESTORE DMCB                               
         GOTO1 (RF),DMCB                                                        
*                                                                               
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FAXBUF   CSECT                                                                  
         DC    14400X'00'                                                       
*                                                                               
FAXBUF2  CSECT                                                                  
         DC    14400X'00'                                                       
*                                                                               
WRKRECA  CSECT                                                                  
         DC    18432X'00'                                                       
*                                                                               
SERTBL#  CSECT                                                                  
         DC    1201X'00'                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
*******  PRINT OFF                                                              
*                                                                               
       ++INCLUDE FAFACTS                                                        
*                                                                               
       ++INCLUDE T413WKA                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLFILE                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE T413WKB                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*                                                                               
WRKIOD   DSECT                                                                  
       ++INCLUDE DDWRKIOD          DSECT FOR WORKER FILE                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDLINKD           DSECT FOR DDLINK                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        DSECT FOR TRANSFER CONTROLS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBAL VARIABLES                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPMAPEQUS         MAP EQUATES                                  
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120T41300    11/10/08'                                      
         END                                                                    
