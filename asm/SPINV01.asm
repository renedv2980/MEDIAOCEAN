*          DATA SET SPINV01    AT LEVEL 053 AS OF 07/12/02                      
*********************************************************************           
****                                                                            
****                                                                            
****   THIS PROGRAM IS DEAD - RIP                                               
****   DO NOT BOTHER UPDATING ANYTHING HERE                                     
****   NEW VERSION IN NINV - SPSNV                                              
****                                                                            
****                                                                            
*********************************************************************           
*PHASE T20601B,+0                                                               
         TITLE 'I.C.S. DATA ENTRY - OVERLAY 1'                                  
         PRINT NOGEN                                                            
T20601   CSECT                                                                  
         NMOD1 0,T20601                                                         
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T20601+4096,R8                                                   
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T206FFD,RA                                                       
         LA    R9,ELWORK                                                        
         USING INVELEM,R9                                                       
         OC    ICSFOOT,ICSFOOT                                                  
         BZ    DB50                                                             
         XC    ICSFOOT,ICSFOOT                                                  
         FOUT  ICSFOOTH                                                         
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
DB50     DS    0H                                                               
         CLC   =C'DEL',ICSACT      DELETE                                       
         BE    DEL                                                              
         CLI   ICSACT,C'R'         REQUEST                                      
         BE    REQ                                                              
         CLI   ICSACT,C'L'         LIST                                         
         BE    LIST                                                             
*                                  FOR ALL BUT REQ AND LIST                     
*                                  MAY NEED TO SET HEADLINES                    
         CLC   ICSHD1(4),DSPHD1    TEST HAVE DISP HEADS                         
         BE    DB50T               YES                                          
         MVC   ICSHD1,DSPHD1       NO- MUST SET                                 
         MVC   ICSHD2,DSPHD2                                                    
         MVC   ICSFCHD,DSPHD3      FILM CODE                                    
         CLI   TRAFSW,C'Y'         ONLY IF DOING FILMS                          
         BE    *+10                                                             
         XC    ICSFCHD,ICSFCHD                                                  
         CLI   PSEUDOPT,C'R'       IF RESPONSES                                 
         BNE   *+10                                                             
         MVC   ICSHD2,=C'RESPONSE'                                              
         FOUT  ICSHD1H                                                          
         FOUT  ICSHD2H                                                          
         FOUT  ICSFCHDH                                                         
*                                                                               
         LA    R2,ICSTIMH          AND UNPROTECT FIELDS                         
DB50D    DS    0H                                                               
         CLI   TRAFSW,C'Y'         IF NOT DOING FILMS                           
         BE    DB50E                                                            
         CLI   0(R2),L'ICSFILM+8   SKIP FILM FIELDS                             
         BE    DB50F               (NOT A GREAT WAY TO TEST)                    
*                                                                               
DB50E    DS    0H                                                               
         TM    1(R2),X'20'                                                      
         BZ    DB50F                                                            
         NI    1(R2),X'DF'         UN-PROTECT                                   
         MVI   5(R2),0             AND ZERO LENGTH                              
         FOUT  (R2)                                                             
*                                                                               
DB50F    DS    0H                                                               
         SR    R0,R0               NEXT FIELD                                   
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,ICSDMYH                                                       
         CR    R2,R0                                                            
         BL    DB50D                                                            
*                                                                               
DB50T    DS    0H                                                               
         CLI   ICSACT,C'C'         CHANGE                                       
         BNE   DB55                                                             
         LA    R3,CHGERR                                                        
         OC    ELLIST,ELLIST                                                    
         BZ    ERRORX              CHANGE NOT AFTER A DISPLAY                   
         L     RF,VGETALL                                                       
         BASR  RE,RF                                                            
         CLI   ERRAREA,0                                                        
         BNE   DB62                                                             
         SPACE 3                                                                
*                                  DELETE ELLIST ELEMS FROM BUFFER              
DB51     LA    R3,RECERR2                                                       
         LA    R5,BUFFER+2                                                      
         SR    R0,R0                                                            
         SR    R7,R7                                                            
         SR    R4,R4                                                            
         LA    RE,SAVELMS          AREA FOR SAVED ELEMS                         
         LA    RF,L'INVELEM*NOLINES   CLEAR AREA                                
         XCEF                                                                   
         LA    R2,SAVELMS                                                       
         L     RF,VRECUP                                                        
         LA    R1,DMCB                                                          
         LA    RE,BUFFER                                                        
         ST    RE,DMCB                                                          
         MVI   DMCB,X'FF'                                                       
*                                                                               
DB52     CLI   0(R5),X'B1'                                                      
         BNE   DB53                                                             
         LA    R4,1(R4)                                                         
         CH    R4,ELLIST(R7)       CHECK AGAINST ELLIST                         
         BL    DB54                                                             
         MVC   0(L'INVELEM,R2),0(R5) SAVE ELEM                                  
         LA    R2,L'INVELEM(R2)                                                 
         GOTO1 (RF),(R1),,(R5)     DELETE FROM BUFFER                           
*                                                                               
         LA    R7,2(R7)                                                         
         LH    R6,ELLIST(R7)                                                    
         LTR   R6,R6                                                            
         BZ    DB55                                                             
         B     DB52                                                             
*                                                                               
DB53     DS    0H                                                               
         CLI   0(R5),0                                                          
         BNE   DB54                                                             
         DC    H'0'                ELLIST AND BUFFER NOT IN SYNC                
*                                  REC HAS BEEN CHANGED                         
*                                                                               
DB54     IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DB52                                                             
*                                                                               
         SPACE 3                                                                
DB55     DS    0H                                                               
         CLI   ICSACT,C'T'                                                      
         BE    DB55A                                                            
         CLI   ICSACT,C'D'         DISPLAY                                      
         BNE   DB55B               ADD  OR REQ                                  
*                                  SEE IF CONTINUED DISPLAY                     
         LA    R0,ICSDATH                                                       
         C     R0,LASTFLD                                                       
         BNH   DB55A               DATA ENTERED BELOW HEADLINES                 
         OC    ELLIST,ELLIST                                                    
         BNZ   DISP                CONTINUED                                    
DB55A    MVC   SAVTIM,=HL2'1'      OPTIONS FOR TIM,                             
         MVI   SAVLEN,X'FF'         LEN,PRD,COST,FILMS                          
         MVC   SAVLEN+1(30),SAVLEN                                              
         B     DB56                                                             
DB55B    XC    SAVES,SAVES                                                      
DB56     EQU   *                                                                
         CLI   IDNAME,C'?'         QUESTION MARK                                
         BE    DISP                                                             
         CLI   ICSACT,C'C'                                                      
         BE    *+10                                                             
         XC    ELLIST,ELLIST                                                    
         SR    R7,R7                                                            
         LA    R5,SAVELMS                                                       
DB57     LA    R2,ICSDATH(R7)      SET R2                                       
         LA    R0,ICSDMYH          EOS                                          
         CR    R2,R0                                                            
         BNL   DB59                DONE WITH SCREEN                             
         ST    R5,ELSAVE           SET ADDRESS OF SAVED ELEM                    
         GOTO1 VEDIT,DMCB,GENOLD   EDIT ROUTINE IN BASE PROG                    
         CLI   ERRAREA,0                                                        
         BNE   DB62                                                             
         OC    INVELEM,INVELEM                                                  
         BZ    DB58                                                             
         CLI   ICSACT,C'D'                                                      
         BE    DISP                DISPLAY                                      
         CLI   ICSACT,C'T'                                                      
         BE    TOTAL                                                            
*                                  ADD OR CHANGE-PREPARE FOR WRITE              
*                                  -------------------------------              
*                                                                               
         CLI   BUFFER+2,X'02'      TEST IF ANY IDS IN RECORD                    
         BNE   DB57B               NO                                           
         CLI   IDNAME,C' '         IF SO-ID MUST BE GIVEN                       
         BH    DB57B               FOR ADD OR CHANGE                            
         LA    R3,IDERR                                                         
         LA    R2,ICSOPTNH                                                      
         B     ERRORX                                                           
*                                                                               
DB57B    DS    0H                                                               
         BAS   RE,ADDEL                                                         
         CLI   ERRAREA,0                                                        
         BNE   DB62A                                                            
DB58     LA    R7,LINLGTH(R7)                                                   
         LA    R5,L'INVELEM(R5)    BUMP SAVED ELEM POINTER                      
         B     DB57                                                             
DB59     CLI   BYTE2,0             TEST ANY DATA ENTERED                        
         BNE   DB59A                                                            
         LA    R3,MISSERR                                                       
         LA    R2,ICSDATH                                                       
         B     ERRORX                                                           
*                                                                               
DB59A    DS    0H                                                               
         OC    INVNUM,INVNUM       IF HAVE INVOICE NUMBER                       
         BZ    DB599               MUST SET HDR ELEM PRD AND EST                
         XC    ELSAVE,ELSAVE                                                    
         XC    DUB,DUB                                                          
         LA    R9,BUFFER+2         START OF ELEM BUFFER                         
*                                                                               
DB593    DS    0H                                                               
         CLI   0(R9),0             EOL                                          
         BE    DB598                                                            
         CLI   0(R9),X'05'         HEADER ELEM                                  
         BNE   DB594                                                            
         ST    R9,ELSAVE           SAVE ITS ADDRESS                             
         B     DB597                                                            
*                                                                               
DB594    DS    0H                                                               
         CLI   0(R9),X'B1'         INVOICE ITEM ELEMENT                         
         BNE   DB597                                                            
         XC    FULL,FULL           IN FULL SET PRD(1),PRD2(1),EST(1)            
         MVC   FULL(1),INVPRD      PRD 1                                        
         TM    INVSTAT,X'40'       IF HAVE EST INPUT                            
         BZ    *+14                                                             
         MVC   FULL+2(1),INVPRD2   IT IS IN 2ND PRD                             
         B     DB594F                                                           
         TM    INVSTAT,X'08'       IF HAVE ID (CONTRACT) INPUT                  
         BNZ   DB594F              SKIP IT, AND DON'T USE 2ND PRD               
         MVC   FULL+1(1),INVPRD2   ELSE 2ND PRD IS WHERE IT SHOULD BE           
*                                                                               
DB594F   DS    0H                                                               
         CLI   DUB,0               FIRST TIME                                   
         BNE   *+14                                                             
         MVC   DUB(3),FULL         SET FIRST VALUES                             
         B     DB597                                                            
*                                                                               
         CLC   FULL(2),DUB         TEST HAVE DIFFERENT PROD                     
         BE    *+8                                                              
         OI    DUB+3,X'80'         SET HAVE MULTI PRODS                         
         CLC   FULL+2(1),DUB+2     TEST HAVE DIFFERENT EST                      
         BE    *+8                                                              
         OI    DUB+3,X'40'         SET HAVE MULTI ESTS                          
         B     DB596                                                            
*                                                                               
DB596    DS    0H                                                               
         TM    DUB+3,X'C0'         IF HAVE MULTIPLE VALUES FOR BOTH             
         BO    DB598               STOP CHECKING                                
*                                                                               
DB597    DS    0H                                                               
         ZIC   R0,1(R9)            ELSE NEXT ELEM                               
         AR    R9,R0                                                            
         B     DB593                                                            
*                                                                               
DB598    DS    0H                                                               
         ICM   R5,15,ELSAVE        NOW SET HEADER ELEM                          
         BZ    DB599                                                            
         USING IHDELEM,R5                                                       
         MVI   IHDPRD,0                                                         
         MVI   IHDPRD2,0                                                        
         TM    DUB+3,X'80'         UNLESS HAVE MULTI PRODS                      
         BNZ   *+16                                                             
         MVC   IHDPRD,DUB          SET PROD CODES                               
         MVC   IHDPRD2,DUB+1       PRD2                                         
*                                                                               
         MVI   IHDEST,0                                                         
         TM    DUB+3,X'40'         UNLESS HAVE MULTI ESTS                       
         BNZ   *+10                                                             
         MVC   IHDEST,DUB+2        SET EST CODE                                 
         DROP  R5                                                               
*                                                                               
DB599    DS    0H                  PREPARE TO FLUSH BUFFER                      
         LA    R4,BUFFER+2                                                      
         ST    R4,ANXTOUT                                                       
         ZIC   R4,RSET                                                          
         BCTR  R4,R0                                                            
         MH    R4,=H'10'            10 RECS PER SET                             
         STC   R4,BYTE4            STARTING RECORD NUMBER                       
         LA    R4,10(R4)           UPPER RECORD NUM LIMIT                       
         STC   R4,BYTE3                                                         
         LA    R4,RECWRK                                                        
         MVI   FRSTRCSW,0          FIRST REC SW                                 
*                                                                               
DB60     EQU   *                                                                
         OC    0(4,R4),0(R4)                                                    
         BZ    DB60A                                                            
         MVC   KEY+14(4),0(R4)                                                  
         BAS   RE,GETREC                                                        
         BAS   RE,BUFFOUT                                                       
         LA    RF,PUTREC                                                        
         B     DB60B                                                            
DB60A    DS    0H                                                               
         BAS   RE,BUFFOUT                                                       
         CLC   IOAREA+13(2),=H'27'                                              
         BNH   DB61                DONT ADD NULL REC                            
         CLC   BYTE4,BYTE3         TEST TOO MANY RECORDS                        
         BNH   *+12                                                             
         MVI   ERRAREA,OVFERR                                                   
         B     DB62                                                             
*                                                                               
         LA    RF,ADDREC                                                        
DB60B    EQU   *                                                                
         BASR  RE,RF                                                            
         LA    R4,4(R4)            NEXT REC                                     
         B     DB60                                                             
DB61     XC    ELLIST,ELLIST                                                    
         B     OKEXT                                                            
*                                                                               
DB62     EQU   *                                                                
         L     R2,FULL             RESTORE R2 - ERROR IN EDIT ROUTNE            
DB62A    SR    R3,R3                                                            
         CLI   ERRAREA,X'FF'       DMGR ERROR                                   
         BE    *+8                                                              
         IC    R3,ERRAREA                                                       
         B     ERRORX                                                           
         SPACE 3                                                                
*        DELETE ROUTINE                                                         
         SPACE 2                                                                
*        **NOTE- DELETE NOT ACTIVE -                                            
*                WORK NEEDED IN GETALL, RECWK LIST, AND WRITES                  
*                                                                               
DEL      DS    0H                                                               
         XC    SAVES,SAVES                                                      
         SR    R7,R7               EDIT FIRST LINE                              
         GOTO1 VEDIT,DMCB,GENOLD                                                
         CLI   ERRAREA,0                                                        
         BNE   DB62                HANDLE ERROR                                 
         OC    INVELEM,INVELEM                                                  
         BZ    DB59                HANDLE MISSING DATA                          
*                                                                               
         GOTO1 VGETALL                                                          
         CLI   ERRAREA,0                                                        
         BNE   DB62                                                             
         B     OKEXT                                                            
         EJECT                                                                  
*                                  BUILD RECORDS FROM BUFFER                    
         SPACE 2                                                                
BUFFOUT  NTR1                                                                   
         BAS   RE,CLRIO                                                         
         CLI   INVKEY,0            CATCH BUG OF ADDING 0 KEY REC                
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IOAREA(13),INVKEY                                                
         MVC   IOAREA+9(1),BYTE4   OLD SEQ POS                                  
         CLI   NEWSW,C'Y'                                                       
         BNE   *+16                                                             
         MVC   IOAREA+10(1),BYTE4    NEW SEQ POS                                
         MVC   IOAREA+9(1),SVINVID   AND INV NUMBER                             
         MVC   IOAREA+24(3),=X'B00300'                                          
         CLI   NETSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    IOAREA+24+2,ICTLNET   NET INVOICE                                
         CLI   PSEUDOPT,C'R'                                                    
         BNE   *+8                                                              
         OI    IOAREA+24+2,ICTLRES   RESPONSE INVOICE                           
         CLI   PSEUDOPT,C'M'                                                    
         BNE   *+8                                                              
         OI    IOAREA+24+2,ICTLMCT   MCT INVOICE                                
         IC    R1,BYTE4                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYTE4                                                         
         L     R5,ANXTOUT                                                       
         MVC   IOAREA+13(2),=H'27'                                              
         LA    R3,IOAREA+27                                                     
*                                                                               
         CLI   FRSTRCSW,0          IF FIRST REC                                 
         BNE   BUFFO2                                                           
         MVI   FRSTRCSW,1                                                       
*                                  SET ACTIVITY ELEM                            
         GOTO1 VDATCON,DMCB,(5,0),(3,DUB)   NEED TODAYS DATE                    
         LA    R6,SVACTEL                                                       
         USING ACTVD,R6                                                         
         CLI   SVACTEL,0           DO WE HAVE ONE                               
         BNE   BUFFO1F             YES                                          
*                                                                               
         XC    SVACTEL,SVACTEL     NO CREATE ONE                                
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         OC    0(4,R4),0(R4)       ARE WE ADDING (1ST DA ZERO)                  
         BNZ   BUFFO1F             NO                                           
         MVC   ACTVADDT,DUB        DATE                                         
         MVC   ACTVADID,T206FFD+10   USER                                       
*                                                                               
BUFFO1F  DS    0H                                                               
         CLC   ACTVADDT,DUB        IF SAME DAY AS ADD                           
         BE    BUFFO1M             NO CHANGE                                    
         MVC   ACTVCHDT,DUB        DATE                                         
         MVC   ACTVCHID,T206FFD+10                                              
*                                                                               
BUFFO1M  DS    0H                                                               
         MVC   IOAREA+13(2),=H'47'    NEW START LENGTH                          
         MVC   IOAREA+27(20),SVACTEL                                            
         LA    R3,IOAREA+47                                                     
         DROP  R6                                                               
*                                                                               
BUFFO2   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    BUFFOX              END OF BUFFER                                
         CLI   0(R5),X'F1'         SKIP ANY ACTIVITY ELEM IN BUFFER             
         BE    BUFFO4                                                           
         CLI   0(R5),X'BF'         AND ANY MARKED ELLIST ELEM                   
         BE    BUFFO4                                                           
*                                                                               
         MVC   HALF,IOAREA+13                                                   
         LH    R1,HALF                                                          
         ZIC   R0,1(R5)                                                         
         AR    R1,R0                                                            
         CH    R1,=H'1970'                                                      
         BH    BUFFOX              FULL RECORD                                  
*                                                                               
         GOTO1 VRECUP,DMCB,IOAREA,(R5),(R3)                                     
*                                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
BUFFO4   DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         ST    R5,ANXTOUT                                                       
         B     BUFFO2                                                           
BUFFOX   EQU   *                                                                
         XIT1                                                                   
*  ACTIVITY ELEM                                                                
       ++INCLUDE DDACTIVD                                                       
*                                                                               
T20601   CSECT                                                                  
         EJECT                                                                  
*                   DISPLAY ROUTINE                                             
         SPACE 3                                                                
DISP     DS    0H                                                               
         LA    R7,LINLGTH          CLEAR ALL LINES BUT FIRST                    
         BAS   RE,SCRNCLR                                                       
         L     RF,VGETALL                                                       
         BASR  RE,RF                                                            
         CLI   ERRAREA,0                                                        
         BNE   DB62                                                             
         CLI   IDNAME,C'?'         QUESTION MARK                                
         BNE   DP08                                                             
*                                  LIST ID'S IN COST FIELD                      
*                                                                               
         LA    R9,BUFFER+2                                                      
         LA    R7,ICSCOSTH                                                      
         MVC   BYTE,IDNAME+1                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   R4,BYTE             NUMBER TO START AT                           
         BCTR  R4,R0                                                            
*                                                                               
DP02     DS    0H                                                               
         CLI   0(R9),X'02'         ID ELEM                                      
         BE    DP04                                                             
         CLI   0(R9),0                                                          
         BE    OKEXT                                                            
DP03     DS    0H                                                               
         ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     DP02                                                             
*                                                                               
DP04     DS    0H                                                               
         LTR   R4,R4                                                            
         BNP   *+10                                                             
         BCTR  R4,R0                                                            
         B     DP03                                                             
         MVC   8(12,R7),2(R9)      ID NAME                                      
         FOUT  (R7)                                                             
         LA    R7,LINLGTH(R7)      NEXT LINE                                    
         LA    R0,ICSDMYH          EOS                                          
         CR    R7,R0                                                            
         BL    DP03                                                             
         B     OKEXT                                                            
*                                                                               
DP08     DS    0H                                                               
*                                  HALF = LAST ELEMENT PREVIOUSLY               
*                                  DISPLAYED (IF ANY)                           
         MVC   HALF,ELLIST+(NOLINES-1)*2                                        
         XC    ELLIST,ELLIST                                                    
         CLI   IDNUM,0                                                          
         BE    *+10                                                             
         MVC   SAVPRD2,IDNUM       ID FILTER                                    
         LA    R2,ICSDATH                                                       
         SR    R7,R7                                                            
         LA    R6,ELLIST                                                        
         LA    R9,BUFFER+2                                                      
         SR    R0,R0                                                            
         SR    R4,R4                                                            
DP2      CLI   0(R9),0                                                          
         BE    DP7                                                              
         CLI   0(R9),X'B1'                                                      
         BNE   DP6                                                              
         LA    R4,1(R4)                                                         
         CH    R4,HALF             TEST ALREADY DISPLAYED                       
         BNH   DP6                                                              
         CLC   INVDAT,SAVDAT                                                    
         BL    DP6                                                              
         BH    DP2A                                                             
         CLC   INVTIM,SAVTIM       TIME                                         
         BL    DP6                                                              
DP2A     CLI   SAVLEN,X'FF'        LENGTH FILTER                                
         BE    DP3                                                              
         CLC   INVLEN,SAVLEN                                                    
         BNE   DP6                                                              
DP3      CLI   SAVPRD,X'FF'                                                     
         BE    DP4                                                              
         CLC   INVPRD,SAVPRD                                                    
         BNE   DP6                                                              
DP4      DS    0H                                                               
         CLC   =C'NONE',IDNAME                                                  
         BNE   *+12                                                             
         TM    INVSTAT,X'08'                                                    
         BNZ   DP6                                                              
         CLI   SAVPRD2,X'FF'                                                    
         BE    DP4B                                                             
         CLC   INVPRD2,SAVPRD2                                                  
         BNE   DP6                                                              
         CLI   IDNUM,0             IF BY ID                                     
         BE    DP4A3                                                            
         TM    INVSTAT,X'08'       ITEM MUST BE ALSO                            
         BZ    DP6                                                              
         B     DP4B                                                             
*                                                                               
DP4A3    DS    0H                                                               
         TM    SAVSTAT,X'40'       TEST FILTER IS EST                           
         BZ    DP4A4                                                            
         TM    INVSTAT,X'40'       YES, THEN SO MUST LINE                       
         BZ    DP6                                                              
         B     DP4B                                                             
*                                                                               
DP4A4    DS    0H                                                               
         TM    INVSTAT,X'40'       NO, THEN NEITHER CAN LINE                    
         BNZ   DP6                                                              
*                                                                               
DP4B     CLC   SAVCOST,=4X'FF'          COST FILTER                             
         BE    DP4D                                                             
         MVC   FULL(1),INVCOSTX         INVCOST EXTENSION (1ST BYTE)            
         MVC   FULL+1(3),INVCOST                                                
         CLC   FULL,SAVCOST                                                     
         BE    DP4D                                                             
         CLC   SAVCOST,ZVAL4       TEST SPECIAL ZERO COST CODE                  
         BNE   DP6                                                              
         OC    FULL,FULL                                                        
         BNZ   DP6                                                              
DP4D     DS    0H                                                               
         CLC   SAVFLM1(2),=3X'FF'       FILM FILTER                             
         BE    DP5                                                              
         XC    WORK(2),WORK                                                     
         CLI   INVELEM+1,13        NO FILMS                                     
         BE    DP4F                                                             
         MVC   WORK(2),INVFILM                                                  
*                                                                               
DP4F     DS    0H                                                               
         CLC   WORK(2),SAVFLM1                                                  
         BNE   DP6                                                              
*                                                                               
DP5      DS    0H                                                               
         LA    RF,ICSDMYH                                                       
         CR    R2,RF                                                            
         BNL   DP8                 AT END OF SCREEN SEE IF ANY MORE             
*                                  TO BE DISPLAYED                              
         BAS   RE,FMT                                                           
         STH   R4,0(R6)                 SAVE COUNTER IN ELLIST                  
         LA    R6,2(R6)                                                         
DP5A     LA    R7,LINLGTH(R7)                                                   
         LA    R2,ICSDATH(R7)                                                   
DP6      IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     DP2                                                              
DP7      EQU   *                                                                
*                                                                               
DP7A     LA    R3,ITMERR                                                        
         LTR   R7,R7                                                            
         BNZ   DP7C                                                             
         XC    ICSOPTN,ICSOPTN     CLEAR REPSONSE/MCT/ETC                       
         FOUT  ICSOPTNH                                                         
         B     ERRORX              IF NO ITEMS FOUND                            
*                                                                               
DP7C     DS    0H                                                               
         LA    R2,ICSMEDH                                                       
         BAS   RE,TOTLIN                                                        
         B     OKEXT                                                            
*                                  MORE TO BE DISPLAYED                         
DP8      XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(L'MORMSG),MORMSG                                          
         FOUT  ICSMSGH                                                          
         OI    ICSACTH+6,X'01'     SET INPUT NEXT TIME                          
         FOUT  ICSACTH                                                          
         LA    R2,ICSACTH                                                       
         MVI   ERRAREA,X'FF'       PREVENT RESET OF CURSOR BY BASE              
         BAS   RE,TOTLIN                                                        
         B     REQ                                                              
*                                  ACTION COMPLETE                              
OKEXT    XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(L'OKMSG),OKMSG                                            
         FOUT  ICSMSGH                                                          
         OI    ICSACTH+6,X'01'     SET INPUT NEXT TIME                          
         FOUT  ICSACTH                                                          
         LA    R2,ICSACTH                                                       
         B     REQ                                                              
OKMSG    DC    C'** ACTION COMPLETED **'                                        
MORMSG   DC    C'** HIT ENTER TO CONTINUE DISPLAY**'                            
ZVAL4    DC    X'80000000'           SPECIAL ZERO COST VALUE                    
         EJECT                                                                  
*                   TOTAL ROUTINE                                               
         SPACE 3                                                                
TOTAL    DS    0H                                                               
         L     RF,VGETALL                                                       
         BASR  RE,RF                                                            
         CLI   ERRAREA,0                                                        
         BNE   DB62                                                             
*                                                                               
         LA    R7,LINLGTH                                                       
         BAS   RE,SCRNCLR                                                       
         BAS   RE,TOTLIN                                                        
         B     OKEXT                                                            
         SPACE 2                                                                
TOTLIN   NTR1                                                                   
         SPACE 2                                                                
         MVC   ICSFOOT+2(16),=C'NUMBER OF SPOTS='                               
         MVC   ICSFOOT+24(14),=C'INVOICE TOTAL='                                
         LA    R1,ICSFOOT+18                                                    
         EDIT  (B4,TSPOTS),(4,(R1))                                             
         SPACE 1                                                                
         LA    R4,ICSFOOT+38                                                    
         CLI   PSEUDOPT,C'R'       SPECIAL FOR RESPONSES                        
         BNE   TL1                                                              
         EDIT  (B4,TDOLLS),(15,(R4)),COMMAS=YES                                 
         B     TL2B                                                             
TL1      DS    0H                                                               
         CLI   CENTS,C'Y'                                                       
         BE    TL2                                                              
         EDIT  (B4,TDOLLS),(15,(R4)),MINUS=YES,FLOAT=$,COMMAS=YES               
         B     TL2B                                                             
TL2      DS    0H                                                               
         EDIT (B4,TDOLLS),(15,(R4)),2,MINUS=YES,FLOAT=$,COMMAS=YES              
TL2B     DS    0H                                                               
         FOUT  ICSFOOTH                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                  REQUEST                                      
         SPACE 2                                                                
REQ      DS    0H                                                               
         MVC   X(6),RQMOS          SET MOS FOR THIS REQ IN X                    
         CLI   RQMOS,C' '          USE GIVEN MOS                                
         BH    *+10                                                             
         MVC   X(4),SUNDATE        OR SUNDATE(4)                                
         CLI   X,C' '              MUST HAVE SOME MOS                           
         BNH   EXIT                                                             
         CLI   HLCHG,C'Y'          TEST HEADLINE CHANGE                         
         BE    REQ1F               YES- OK                                      
         CLC   LRQMOS,X            NO- TEST MOS CHANGE                          
         BE    EXIT                NO- DONE                                     
*                                                                               
REQ1F    DS    0H                                                               
         CLI   RQSW,C'Y'           TEST SPECIFICALLY REQUESTED                  
         BE    REQ2                                                             
         CLI   REQPROF+0,C'Y'      TEST AUTO REQ FOR $INV                       
         BNE   EXIT                NO REQ                                       
         CLI   ICSACT,C'D'         BUT SKIP FOR ACTION DISPLAY                  
         BE    EXIT                                                             
         CLI   ICSACT,C'T'         AND TOTAL                                    
         BE    EXIT                                                             
*                                                                               
REQ2     DS    0H                                                               
         MVC   LRQMOS,X            SAVE MOS FOR THIS REQ                        
         LA    R7,IOAREA                                                        
         USING QRECD,R7                                                         
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QCODE,=C'U2'                                                     
         CLI   RQREP,C' '                                                       
         BNH   *+10                                                             
         MVC   QCODE,RQREP                                                      
         MVC   QAGY,AGYALPHA                                                    
         MVC   QMED,ICSMED                                                      
         MVC   QCLT,ICSCLT                                                      
         OC    QCLT,=3C' '                                                      
         MVC   QPROD,RQPRD                                                      
         CLI   RQPRD,C' '                                                       
         BH    REQ3D                                                            
         CLI   REQPROF+2,C'A'      IF NO PRD, TEST PROFILE                      
         BE    REQ3B               PRD = ALL                                    
         MVC   QPROD,=C'POL'       ELSE POL IF PRESENT                          
         L     RF,APRDLST                                                       
REQ3     DS    0H                                                               
         CLI   3(RF),0                                                          
         BE    REQ3B                                                            
         CLI   3(RF),X'FF'                                                      
         BE    REQ3D                                                            
         LA    RF,4(RF)                                                         
         B     REQ3                                                             
*                                                                               
REQ3B    DS    0H                                                               
         MVC   QPROD,=C'ALL'       ELSE ALL                                     
REQ3D    DS    0H                                                               
         CLI   RQPRD+3,C' '                                                     
         BNH   REQ3F                                                            
         LA    RF,QPROD2           PRD2                                         
         CLI   RQPRD+3,C'0'                                                     
         BL    *+8                                                              
         LA    RF,QEST             OR EST                                       
         MVC   0(3,RF),RQPRD+3                                                  
*                                                                               
REQ3F    DS    0H                                                               
         MVC   HALF,BMS                                                         
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   QSTA,ICSSTA                                                      
         CLI   QSTA+3,C'-'                                                      
         BNE   *+12                                                             
         MVI   QSTA+3,C' '                                                      
         B     REQ6                                                             
         CLI   QSTA+4,C'/'                                                      
         BE    REQ6                                                             
         CLI   QSTA+4,C'A'         KEEP X OF WABCX                              
         BNL   REQ6                                                             
         MVC   QSTA+4(1),ICSSTA+5  WABC-X                                       
REQ6     EQU   *                                                                
         OC    QSTA,=6C' '                                                      
         CLI   RQMOS,C' '          IF MOS SPECIFICALLY REQ'D                    
         BNH   *+14                                                             
         MVC   QSTART(6),RQMOS     USE IT                                       
         B     *+10                                                             
         MVC   QSTART(4),SUNDATE   ELSE USE SUNDATE(4)                          
*                                                                               
         CLI   RQBOOK,C' '         TEST BOOK SPECIFIED                          
         BNH   REQ7B                                                            
         CLI   RQBOOK,X'FF'        TEST BOOK=NO                                 
         BE    REQ8M                                                            
         MVC   QBOOK1(6),RQBOOK                                                 
         B     REQ8M                                                            
*                                                                               
REQ7B    DS    0H                  NO BOOK GIVEN                                
         CLI   REQPROF+3,C'N'      TEST HUT OPTION                              
         BNE   *+10                                                             
         MVC   QBOOK1+4(2),=C'NO'                                               
         MVC   X(6),QSTART         USE MOS                                      
         CLI   X+4,C' '            IF FULL DATE GIVEN                           
         BNH   REQ7D                                                            
         GOTO1 VADDAY,DMCB,X,X,6    ADD 6 DAYS TO ENSURE                        
*                                  RIGHT CALENDAR MONTH                         
REQ7D    DS    0H                                                               
         PACK  DUB,X+2(2)          LOOK UP BOOK BASED ON MOS MONTH              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         IC    R0,REQPROF+3(R1)    BOOKS IN REQPROF+4 THRU +15                  
         LTR   R0,R0               TEST VALUE PRESENT                           
         BZ    REQ8M               NO - NO BOOK                                 
         CH    R0,=H'13'           MONTH 13 = ACT BOOK                          
         BNE   REQ8F                                                            
         MVC   QBOOK1(4),=C'ACT '                                               
         B     REQ8M                                                            
*                                                                               
REQ8F    DS    0H                                                               
         MVI   BYTE,0                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1+2(2),DUB     SET MONTH                                    
         MVC   QBOOK1(2),X         SET YEAR = MOS YEAR                          
         CR    R0,R1               UNLESS BOOK GT MOS                           
         BNH   REQ8J                                                            
*                                                                               
REQ8H    DS    0H                                                               
         PACK  DUB,QBOOK1(2)                                                    
         SP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1(2),DUB                                                    
*                                               SEE IF BOOK EXISTS              
REQ8J    DS    0H                                                               
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,WORK,(3,WORK)                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),ICSMED                                                  
         MVI   KEY+2,C'N'          NIELSON                                      
         CLI   SVCLPROF+3,C'0'                                                  
         BE    REQ8L                                                            
         CLC   WORK(2),=X'5E01'    FOR JAN94+ ALWAYS NIELSEN                    
         BNL   REQ8L                                                            
         MVI   KEY+2,C'A'          ELSE ARB                                     
REQ8L    DS    0H                                                               
         MVC   KEY+3(2),WORK                                                    
         XC    KEY+3(2),=X'FFFF'                                                
*                                                                               
         LA    R3,BKERR2                                                        
         LA    R2,ICSREQH                                                       
*                                  NOTE- USE IOAREA+200 BECAUSE                 
*                                       REQUEST BUILT AT IOAREA                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA+200,     X        
               DMWORK                                                           
         TM    DMCB+8,X'FF'                                                     
         BNZ   ERRORX                                                           
         CLC   KEY(5),IOAREA+200                                                
         BE    REQ8M               BOOK OK                                      
*                                                                               
         CLI   BYTE,0              NOT ON FILE, TRY TO BACK UP 1 YEAR           
         BNE   ERRORX              ALREADY HAVE DONE                            
         MVI   BYTE,1                                                           
         B     REQ8H                                                            
*                                                                               
REQ8M    DS    0H                                                               
         MVC   QUESTOR(10),RQNAME                                               
         CLI   RQBUYOPT,C' '                                                    
         BNH   *+10                                                             
         MVC   QOPT1,RQBUYOPT                                                   
*                                                                               
         CLI   PSEUDOPT,C'R'       RESPONSE                                     
         BNE   *+8                                                              
         MVI   QAREA+32,C'R'                                                    
         CLI   PSEUDOPT,C'M'       MTC                                          
         BNE   *+8                                                              
         MVI   QAREA+32,C'M'                                                    
*                                                                               
         PACK  DUB,QCODE                                                        
         CVB   R0,DUB                                                           
         STC   R0,QCTL+10                                                       
         MVI   QCTL+14,106                                                      
         IC    R0,TERMNAL                                                       
*                                                                               
         CLI   QSTA+4,C'/'         FOR LOCAL CABLE                              
         BNE   REQ9H                                                            
         MVI   QREC2,C' '          NEED 2ND CARD FOR NETWORK                    
         MVC   QREC2+1(L'QREC2-1),QREC2                                         
         MVC   QCBLNET,ICSSTA+5                                                 
         OC    QCBLNET,=3C' '                                                   
         CLI   QCBLNET,C' '                                                     
         BNH   REQ9H                                                            
         MVI   QCONT,C'*'          CONTINUED INDICATOR                          
         OI    QCTL+15,X'10'       SET HAVE 2ND REQ                             
         MVI   QCTL+14,186                                                      
*                                                                               
REQ9H    DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',QCTL,QCTL,((R0),0)           
         SPACE 1                                                                
         TM    8(R1),X'FF'                                                      
         BZ    REQ10                                                            
         SR    R3,R3                                                            
         B     ERRORX                                                           
REQ10    EQU   *                                                                
         MVI   HLCHG,C'N'                                                       
         CLI   ICSACT,C'R'                                                      
         BNE   REQ11                                                            
         XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(23),=C'** REQUEST GENERATED **'                           
         FOUT  ICSMSGH                                                          
         B     EXIT                                                             
REQ11    DS    0H                                                               
         MVC   WORK(L'ICSMSG),ICSMSG                                            
         LA    R4,WORK+L'ICSMSG-1                                               
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(5,R4),=C'(REQ)'                                                
         MVC   ICSMSG,WORK                                                      
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                   FORMAT SCREEN LINE                                          
         SPACE 3                                                                
FMT      NTR                                                                    
         SPACE 2                                                                
*                                  DATE                                         
         LA    R2,ICSDATH(R7)                                                   
         GOTO1 VDATCON,DMCB,(2,INVDAT),(5,8(R2))                                
*                                                                               
         SPACE 1                                                                
         MVI   7(R2),8                                                          
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  (R2)                                                             
*                                  TIME                                         
         LA    R2,ICSTIMH(R7)                                                   
         MVC   FULL,INVTIM                                                      
         LH    R0,FULL                                                          
         CH    R0,=H'2400'                                                      
         BNH   *+12                                                             
         SH    R0,=H'2400'         ADJUST FOR TIME GREATER 2400                 
         STH   R0,FULL                                                          
         GOTO1 VTIMUNPK,(R1),FULL,WORK                                          
         SPACE 1                                                                
         MVI   7(R2),5             LENGTH = 5                                   
         CLI   WORK,X'40'                                                       
         BNE   FMT2A                                                            
         MVC   WORK(4),WORK+1                                                   
         MVI   WORK+4,0                                                         
         MVI   7(R2),4             LENGTH = 4                                   
*                                                                               
FMT2A    DS    0H                                                               
         LA    RF,8(R2)                                                         
         TM    INVSTAT,X'20'       INTERVAL ACCEPT OPTION                       
         BZ    *+16                                                             
         MVI   0(RF),C'N'                                                       
         LA    RF,1(RF)                                                         
         B     FMT2A4                                                           
*                                                                               
         TM    INVSTAT,X'02'       IGNORE TIME FOR MATCH OPT                    
         BZ    *+12                                                             
         MVI   0(RF),C'T'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
FMT2A4   DS    0H                                                               
         MVC   0(5,RF),WORK                                                     
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  (R2)                                                             
         SPACE 1                                                                
*                                  LENGTH                                       
         LA    R2,ICSLENH(R7)                                                   
         EDIT  INVLEN,(3,8(R2)),ALIGN=LEFT                                      
         STC   R0,7(R2)                                                         
         OI    4(R2),X'08'         SET NUMERIC BIT - SO THAT IF FIELD           
*                                  IS NOT RE-TRANSMITTED WILL STILL             
*                                  BE ABLE TO PROCESS                           
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  (R2)                                                             
*                                  PRODUCT(S)                                   
         LTR   R7,R7               IF FIRST LINE                                
         BNZ   *+10                                                             
         MVC   BLRPRD,INVPRD       SAVE PRODUCT                                 
         LA    R2,ICSPRDH(R7)                                                   
         MVI   8(R2),X'40'         CLEAR PRD                                    
         MVC   9(L'ICSPRD-1,R2),8(R2)                                           
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  (R2)                                                             
         CLI   SAVPRD,X'FF'                                                     
         BNE   FMT2B                                                            
         XC    SAVPRDN(3),SAVPRDN                                               
         LA    R4,INVPRD                                                        
         LA    R5,SAVPRDN                                                       
         BAS   RE,GETPRD                                                        
*                                                                               
FMT2B    DS    0H                                                               
         CLI   SAVPRD2,X'FF'                                                    
         BNE   FMT3                                                             
         TM    INVSTAT,X'40'       EST NOT PRD                                  
         BZ    FMT2C                                                            
         LTR   R7,R7               IF FIRST LINE                                
         BNZ   *+10                                                             
         MVC   BLREST,INVPRD2      SAVE PRODUCT                                 
         ZIC   R0,INVPRD2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK SAVPRD2N,DUB                                                      
         B     FMT3                                                             
FMT2C    DS    0H                                                               
         CLI   FILMOPT,C'Y'                                                     
         BNE   FMT2F                                                            
         XC    SAVPRD2N,SAVPRD2N                                                
         MVC   SAVPRD2N(1),INVPRD2                                              
         B     FMT3                                                             
FMT2F    DS    0H                                                               
         LA    R4,INVPRD2                                                       
         LA    R5,SAVPRD2N                                                      
         BAS   RE,GETPRD                                                        
FMT3     MVC   8(3,R2),SAVPRDN     PRODUCT MNEMOMIC                             
         CLI   INVPRD2,0                                                        
         BE    FMT3A                                                            
         TM    INVSTAT,X'08'       SKIP IF BY ID                                
         BNZ   FMT3A                                                            
         MVI   11(R2),C'-'                                                      
         MVC   12(3,R2),SAVPRD2N   2ND PROD. MNEMONIC                           
*                                  COST                                         
*                                                                               
*                                  DETERMINE LENGTH OF FIELD                    
FMT3A    LA    R5,7                                                             
*                                  GO BACKWARDS THRU FIELD                      
FMT3B    LA    R4,7(R2,R5)                                                      
         CLI   0(R4),X'40'                                                      
         BNE   *+8                                                              
         BCT   R5,FMT3B                                                         
         STC   R5,7(R2)            SET LENGTH                                   
FMT4     LA    R2,ICSCOSTH(R7)                                                  
         LA    R3,8(R2)                                                         
         TM    INVSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   0(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
         TM    INVSTAT,X'04'       MG IND                                       
         BZ    *+12                                                             
         MVI   0(R3),C'M'                                                       
         LA    R3,1(R3)                                                         
         MVC   FULL(1),INVCOSTX    INVCOST EXTENSION (1ST BYTE)                 
         MVC   FULL+1(3),INVCOST                                                
         L     R0,FULL                                                          
         CLI   PSEUDOPT,C'R'       SPECIAL FOR RESPONSES                        
         BNE   FMT4B                                                            
         EDIT  (R0),(11,0(R3)),COMMAS=YES,ALIGN=LEFT                            
         B     FMT5B                                                            
*                                                                               
FMT4B    DS    0H                                                               
         TM    INVSTAT,X'01'                                                    
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         CLI   CENTS,C'Y'                                                       
         BE    FMT5                                                             
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         MVI   0(R3),C'0'          SHOW ZERO (EDIT HAS BUG)                     
         LA    R0,1                SET LENGTH                                   
         B     FMT5B                                                            
         EDIT  (R0),(11,0(R3)),ALIGN=LEFT,FLOAT=-                               
         B     FMT5B                                                            
FMT5     DS    0H                                                               
         EDIT (R0),(11,0(R3)),2,ALIGN=LEFT,FLOAT=-                              
FMT5B    DS    0H                                                               
         SPACE 1                                                                
         TM    INVSTAT,X'01'                                                    
         BZ    *+6                 IF COST IS POSITIVE                          
         BCTR  R0,R0               REDUCE LENGTH BY 1                           
         TM    INVSTAT,X'80'                                                    
         BZ    *+8                                                              
         AH    R0,=H'1'            UP LENGTH BY 1 FOR 'N'                       
         TM    INVSTAT,X'04'       AND FOR MG                                   
         BZ    *+8                                                              
         AH    R0,=H'1'                                                         
         STC   R0,7(R2)            SET LENGTH                                   
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  (R2)                                                             
*                                  FORMAT FILM CODES                            
         LA    R2,ICSFILMH(R7)                                                  
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BNZ   FMTEXT                                                           
*                                                                               
         XC    8(L'ICSFILM,R2),8(R2)                                            
         MVC   8(4,R2),=C'NONE'                                                 
         LA    R3,8+4(R2)          R3 IS POS FOR BILLBOARD (-B)                 
         CLI   INVELEM+1,13        NO FILMS                                     
         BNH   FMT6                                                             
*                                                                               
         CLI   INVFILM,0                                                        
         BE    FMT6                                                             
         LA    R5,INVFILM                                                       
         LA    R6,8(R2)                                                         
         BAS   RE,FMTFLM                                                        
         LA    R3,8+8(R2)                                                       
*                                                                               
         CLI   INVFILM2,0                                                       
         BE    FMT6                                                             
         MVI   16(R2),C'-'                                                      
         LA    R5,INVFILM2                                                      
         LA    R6,17(R2)                                                        
         BAS   RE,FMTFLM                                                        
         LA    R3,8+17(R2)                                                      
*                                                                               
FMT6     DS    0H                                                               
         TM    INVSTAT,INVSBLBQ    TEST BILLBOARD                               
         BZ    *+10                                                             
         MVC   0(2,R3),=C'-B'                                                   
*                                                                               
         TM    INVSTAT2,X'80'      IF SET TO IGNORE FILM ERRS                   
         BZ    FMT7                                                             
         MVC   WORK(L'ICSFILM-2),8(R2)     MOVE EVERYTHING OVER 2               
         MVC   8+2(L'ICSFILM-2,R2),WORK                                         
         MVC   8(2,R2),=C'N-'              AND APPEND 'N-'                      
*                                                                               
FMT7     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
FMTEXT   XIT                                                                    
         SPACE 2                                                                
FMTFLM   DS    0H                                                               
         LA    R4,BUFFER+2                                                      
FF4      DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    FF9                                                              
         CLI   0(R4),X'F0'         SKIP SPECIAL ELEMS                           
         BNL   FF6                                                              
         CLI   0(R4),4                                                          
         BL    FF6                                                              
         BH    FF9                                                              
         CLC   0(1,R5),2(R4)       TEST FILM ID                                 
         BNE   FF6                                                              
         MVC   0(8,R6),3(R4)       SET FILM CODE                                
         BR    RE                                                               
*                                                                               
FF6      DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FF4                                                              
*                                                                               
FF9      DS    0H                                                               
         MVC   0(8,R6),=C'**ERROR*'                                             
         BR    RE                                                               
         EJECT                                                                  
*        INVOICE LISTING ROUTINE                                                
         SPACE 2                                                                
LIST     DS    0H                                                               
         LA    R2,ICSDATH          POINT TO FIRST DATE FIELD                    
         TM    4(R2),X'80'         TEST INPUT THIS TIME                         
         BNZ   LS1R                YES, EDIT DATE                               
*                                  ELSE SEE WHERE CURSOR IS                     
         L     R1,SYSPARMS                                                      
         L     R2,0(R1)            A(TIO)                                       
         USING TIOBD,R2                                                         
         SR    R4,R4                                                            
         ICM   R4,3,TIOBCURD       RELATIVE CURSOR POS                          
         DROP  R2                                                               
         A     R4,VTWA                                                          
         LA    R2,ICSDATH          TEST VS FIRST INVOICE LINE                   
         CR    R4,R2                                                            
         BL    LS1T                IF CURSOR ABOVE, CONTINUE LIST               
*                                  ELSE FIND WHAT LINE IT IS ON                 
         LA    R2,LINLGTH(R2)                                                   
LS1M     DS    0H                                                               
         CR    R4,R2                                                            
         BL    LS1Q                                                             
LS1P     DS    0H                                                               
         LA    R2,LINLGTH(R2)                                                   
         B     LS1M                                                             
*                                                                               
LS1Q     DS    0H                                                               
         XC    LISTKEY,LISTKEY     CLEAR LIST START POINT                       
         XC    ELLIST,ELLIST                                                    
         MVI   SVINVID,0                                                        
         SH    R2,=Y(LINLGTH)                                                   
         LA    RF,ICSDATH                                                       
         MVC   8(L'ICSDAT,RF),8(R2)     MOVE SELECTED MONTH                     
         FOUT  (RF)                                                             
         MVI   ICSDATH+5,6         SET INPUT LENGTH                             
         NI    ICSDATH+4,X'DF'     UNVALIDATE                                   
         OI    ICSDATH+4,X'80'     SET INPUT THIS TIME                          
         LA    RF,ICSINVH               AND INVOICE NUBMER                      
         LA    R2,ICSFILMH-ICSDATH(R2)  INVOICE IS IN FILM FIELD                
         XC    INVNUM,INVNUM       'NONE' INVOICE                               
         XC    ICSINV,ICSINV                                                    
         CLC   =C'NONE',8(R2)                                                   
         BE    LS1Q4                                                            
*                                  REMOVE RESP/PSEUDO IF PRESENT                
         LA    RE,8(R2)                                                         
         LA    R0,L'ICSFILM-6                                                   
*                                                                               
LSIQ6    DS    0H                                                               
         CLC   0(6,RE),=C'(RESP)'                                               
         BNE   *+14                                                             
         MVC   0(6,RE),=6C' '                                                   
         B     LSIQ8                                                            
         CLC   0(5,RE),=C'(MCT)'                                                
         BNE   *+14                                                             
         MVC   0(6,RE),=6C' '                                                   
         B     LSIQ8                                                            
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,LSIQ6                                                         
*                                                                               
LSIQ8    DS    0H                                                               
         MVC   8(L'ICSINV,RF),8(R2)                                             
         MVC   INVNUM,8(R2)                                                     
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'INVNUM-1),WORK                                          
         OC    INVNUM,WORK         OR WITH SPACES                               
*                                                                               
LS1Q4    DS    0H                                                               
         FOUT  (RF)                                                             
         LA    RF,ICSACTH                                                       
         MVC   8(L'ICSACT,RF),=C'DISPLAY     '                                  
         FOUT  (RF)                                                             
         MVI   RSET,1              FIRST RECORD SET                             
*                                                                               
         XC    ICSTIM,ICSTIM       CLEAR FIRST INPUT LINE                       
         FOUT  ICSTIMH                                                          
         XC    ICSLEN,ICSLEN                                                    
         FOUT  ICSLENH                                                          
         XC    ICSPRD,ICSPRD                                                    
         FOUT  ICSPRDH                                                          
         XC    ICSCOST,ICSCOST                                                  
         FOUT  ICSCOSTH                                                         
         XC    ICSFILM,ICSFILM                                                  
         FOUT  ICSFILMH                                                         
         XC    ICSOPTN,ICSOPTN                                                  
         FOUT  ICSOPTNH                                                         
         B     DB50                START FROM THE TOP                           
*                                                                               
*                                                                               
LS1R     DS    0H                  EDIT FOR DATE                                
         XC    LISTKEY,LISTKEY     CLEAR LIST START POINT                       
         XC    SAVES,SAVES                                                      
         SR    R7,R7               EDIT FIRST LINE                              
         GOTO1 VEDIT,DMCB,GENOLD                                                
         CLI   ERRAREA,0                                                        
         BNE   DB62                HANDLE ERROR                                 
*                                                                               
LS1T     DS    0H                  SET HEADLINES                                
         CLC   ICSHD1(4),LSTHD1    TEST HAVE LIST HEADS                         
         BE    LS2T                YES                                          
         MVC   ICSHD1,LSTHD1       NO- MUST SET                                 
         MVC   ICSHD2,LSTHD2                                                    
         MVC   ICSFCHD,LSTHD3                                                   
         FOUT  ICSHD1H                                                          
         FOUT  ICSHD2H                                                          
         FOUT  ICSFCHDH                                                         
*                                                                               
         LA    R2,ICSTIMH          AND PROTECT FIELDS                           
LS2D     DS    0H                  (LEAVE FIRST DATE UNPROTECTED)               
         TM    1(R2),X'20'                                                      
         BNZ   LS2F                                                             
         OI    1(R2),X'20'         PROTECT                                      
         FOUT  (R2)                                                             
*                                                                               
LS2F     DS    0H                                                               
         SR    R0,R0               NEXT FIELD                                   
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,ICSDMYH                                                       
         CR    R2,R0                                                            
         BL    LS2D                                                             
*                                                                               
LS2T     DS    0H                                                               
         SR    R7,R7               START ON FIRST LINE                          
         BAS   RE,SCRNCLR                                                       
         XC    LISTKTB,LISTKTB     CLEAR TABLE OF KEYS                          
         LA    R3,LISTKTB                                                       
         OC    LISTKEY,LISTKEY     IF HAVE KEY FROM BEFORE                      
         BZ    LS3                                                              
         MVC   KEY,LISTKEY                                                      
         XC    LISTKEY,LISTKEY     CLEAR SAVED KEY                              
         B     LS3B                                                             
*                                                                               
LS3      DS    0H                                                               
         MVC   KEY(13),INVKEY                                                   
LS3B     DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
LS4      DS    0H                                                               
         BAS   RE,SEQ                                                           
         CLC   KEY(07),KEYSAVE     CHECK THRU STATION                           
         BNE   LS34                                                             
         CLI   KEY+10,0            LOOK ONLY AT FIRST RECS (SEQ=0)              
         BNE   LS4                                                              
*                                                                               
         LA    R0,ICSDMYH          TEST ANY MORE ROOM                           
         LA    R2,ICSDATH(R7)                                                   
         CR    R2,R0                                                            
         BL    LS5                                                              
         MVC   LISTKEY,KEY         NO- SAVE NEXT KEY                            
         B     LS32                                                             
*                                                                               
LS5      DS    0H                                                               
         MVC   0(13,R3),KEY        SAVE KEY                                     
         LA    R3,13(R3)                                                        
         BAS   RE,GETREC                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(2,KEY+7),WORK   MOS                                
         GOTO1 VADDAY,DMCB,WORK,WORK,15                                         
         GOTO1 VDATCON,DMCB,WORK,(6,WORK+6)                                     
         LA    R2,ICSDATH(R7)                                                   
         MVC   8(6,R2),WORK+6                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R9,IOAREA+24        LOOK FOR HEADER ELEM                         
*                                                                               
LS6      DS    0H                                                               
         CLI   0(R9),0             EOR                                          
         BE    LS7D                                                             
         CLI   0(R9),X'05'         INVOICE HEADER ELEM                          
         BE    LS8                                                              
         CLI   0(R9),X'B0'         CONTROL ELEM                                 
         BE    LS7B                                                             
*                                                                               
LS7      DS    0H                                                               
         ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     LS6                                                              
*                                                                               
LS7B     DS    0H                                                               
         USING ICTLELEM,R9                                                      
         MVC   SVICTL,ICTLCTL      SAME CONTROL BYTE                            
         B     LS7                                                              
         DROP  R9                                                               
*                                                                               
LS7D     DS    0H                                                               
         LA    R9,X                IF NO HDR ELEM, USE NULLS                    
         XC    X,X                                                              
*                                                                               
LS8      DS    0H                  INVOICE HEADER ELEM                          
         USING IHDELEM,R9                                                       
         LA    R4,IHDPRD           PRODUCT                                      
         LA    R5,WORK                                                          
         MVC   WORK,=C'*  '                                                     
         BAS   RE,GETPRD                                                        
         LA    R2,ICSPRDH(R7)                                                   
         MVC   8(3,R2),WORK                                                     
         CLI   IHDPRD2,0                                                        
         BE    LS8B                                                             
         LA    R4,IHDPRD2                                                       
         BAS   RE,GETPRD                                                        
         MVI   8+3(R2),C'-'                                                     
         MVC   8+4(3,R2),WORK                                                   
LS8B     DS    0H                                                               
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,ICSCOSTH(R7)                                                  
         CLI   IHDEST,0            TEST HAVE ESTIMATE                           
         BNE   *+12                                                             
         MVI   8(R2),C'*'                                                       
         B     LS8C                                                             
         EDIT  (B1,IHDEST),(3,8(R2)),3,FILL=0                                   
*                                                                               
LS8C     DS    0H                                                               
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,ICSFILMH(R7)                                                  
         MVC   8(4,R2),=C'NONE'                                                 
         CLI   IHDINV,C' '                                                      
         BNH   *+10                                                             
         MVC   8(10,R2),IHDINV                                                  
         TM    SVICTL,ICTLRES+ICTLMCT   RESPONSE/MCT INVOICE                    
         BZ    LS8C4                                                            
         LA    RF,8+10(R2)                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(6,RF),=C'(RESP)'                                               
         TM    SVICTL,ICTLRES                                                   
         BO    LS8C4                                                            
         MVC   2(6,RF),=C'(MCT) '                                               
LS8C4    DS    0H                                                               
         FOUT  (R2)                                                             
*                                                                               
LS8D     DS    0H                                                               
         LA    R2,ICSTIMH(R7)                                                   
         MVC   8(4,R2),=C'EASI'                                                 
         CLI   IHDRDT,0            TEST EASI (HAVE RECIEVED DATE)               
         BNE   *+10                                                             
         MVC   8(6,R2),=C'MANUAL'                                               
         FOUT  (R2)                                                             
*                                                                               
LS10     DS    0H                                                               
         TM    KEY+13,X'80'        TEST INVOICE DELETED                         
         BNZ   LS11F                                                            
*                                                                               
         LA    R9,IOAREA+24        SEE IF ANY INVOICE ITEMS                     
LS10B    DS    0H                                                               
         CLI   0(R9),0             EOR                                          
         BE    LS11F                                                            
         CLI   0(R9),X'B1'         ITEM ELEM                                    
         BE    LS12                                                             
         ZIC   R0,1(R9)            NEXT ELEM                                    
         AR    R9,R0                                                            
         B     LS10B                                                            
*                                                                               
LS11F    DS    0H                                                               
         LA    R2,ICSFILM+L'ICSFILM-1(R7)                                       
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(5,R2),=C'(DEL)'                                                
*                                                                               
LS12     DS    0H                                                               
         LA    R7,LINLGTH(R7)      BUMP TO NEXT LINE                            
         B     LS4                 NEXT RECORD                                  
*                                                                               
LS32     DS    0H                  MORE TO BE DISPLAYED                         
         XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(L'MORMSG),MORMSG                                          
         FOUT  ICSMSGH                                                          
         OI    ICSACTH+6,X'01'     SET INPUT NEXT TIME                          
         FOUT  ICSACTH                                                          
         LA    R2,ICSACTH                                                       
         MVI   ERRAREA,X'FF'       PREVENT RESET OF CURSOR BY BASE              
         B     EXIT                                                             
*                                  LIST COMPLETE                                
LS34     XC    ICSMSG,ICSMSG                                                    
         MVC   ICSMSG(L'OKMSG),OKMSG                                            
         FOUT  ICSMSGH                                                          
         OI    ICSACTH+6,X'01'     SET INPUT NEXT TIME                          
         FOUT  ICSACTH                                                          
         LA    R2,ICSACTH                                                       
         B     EXIT                                                             
         SPACE 2                                                                
LSTHD1   DC    CL36'MONTH      SOURCE           PRODUCT '                       
LSTHD2   DC    CL7'EST'                                                         
LSTHD3   DC    CL10'INV NO.'                                                    
         SPACE 2                                                                
DSPHD1   DC    CL36'DATE       TIME      LEN    PRODUCT '                       
DSPHD2   DC    CL7'COST'                                                        
DSPHD3   DC    CL10' FILM CODE'                                                 
         EJECT                                                                  
*                   ERASE SCREEN                                                
         SPACE 3                                                                
SCRNCLR  NTR                                                                    
SC2      LA    R2,ICSDATH(R7)      R7 INDEXES TO FIRST LINE TO CLEAR            
         LA    R0,ICSDMYH          EOS                                          
         CR    R2,R0                                                            
         BNL   SC20                                                             
         OC    8(L'ICSDAT,R2),8(R2)                                             
         BZ    SC3                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSDAT,R2),8(R2)                                             
         FOUT  (R2)                                                             
SC3      LA    R2,ICSTIMH(R7)                                                   
         OC    8(L'ICSTIM,R2),8(R2)                                             
         BE    SC4                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSTIM,R2),8(R2)                                             
         FOUT  (R2)                                                             
SC4      LA    R2,ICSLENH(R7)                                                   
         OC    8(L'ICSLEN,R2),8(R2)                                             
         BE    SC5                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSLEN,R2),8(R2)                                             
         FOUT  (R2)                                                             
SC5      LA    R2,ICSPRDH(R7)                                                   
         OC    8(L'ICSPRD,R2),8(R2)                                             
         BE    SC6                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSPRD,R2),8(R2)                                             
         FOUT  (R2)                                                             
SC6      LA    R2,ICSCOSTH(R7)                                                  
         OC    8(L'ICSCOST,R2),8(R2)                                            
         BE    SC7                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSCOST,R2),8(R2)                                            
         FOUT  (R2)                                                             
SC7      LA    R2,ICSFILMH(R7)                                                  
         OC    8(L'ICSFILM,R2),8(R2)                                            
         BZ    SC8                                                              
         MVI   7(R2),0                                                          
         XC    8(L'ICSFILM,R2),8(R2)                                            
         FOUT  (R2)                                                             
SC8      LA    R7,LINLGTH(R7)                                                   
         B     SC2                                                              
SC20     DS    0H                                                               
SC21     XIT                                                                    
         EJECT                                                                  
*                   ELEMENT INSERTION                                           
ADDEL    NTR                                                                    
         SPACE 2                                                                
         LA    R9,ELWORK                                                        
         USING INVELEM,R9                                                       
         SR    R0,R0                                                            
         MVI   INVELEM,X'B1'                                                    
         MVI   INVELEM+1,17        NEW ELEM LENGTH                              
*                                                                               
         LA    R4,BUFFER                                                        
         LA    R5,BUFFER+2                                                      
AE2      CLI   0(R5),0                                                          
         BE    AE4                                                              
         CLI   0(R5),X'F0'         SKIP SPECIAL ELEMS                           
         BNL   AE3                                                              
         CLC   INVELEM(1),0(R5)                                                 
         BL    AE4                                                              
         BH    AE3                                                              
         CLC   INVELEM+2(11),2(R5)                                              
         BL    AE4                                                              
         BH    AE3                                                              
         CLI   I2YPROF+3,C'Y'      ARE DUPES OK?                                
         BE    AE4                                                              
         B     AEDUPE                                                           
AE3      IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     AE2                                                              
AE4      EQU   *                                                                
         GOTO1 VRECUP,DMCB,(X'FF',(R4)),(R9),(R5)                               
         SPACE 1                                                                
         CLC   BUFFER(2),BUFFL                                                  
         BH    AEOVF                                                            
         B     AEEXT                                                            
AEEXT    XIT                                                                    
         SPACE 2                                                                
AEDUPE   MVI   ERRAREA,DUPERR                                                   
         B     AEEXT                                                            
AEOVF    MVI   ERRAREA,OVFERR                                                   
         B     AEEXT                                                            
         EJECT                                                                  
*                   GET PRODUCT CODE                                            
         SPACE 3                                                                
GETPCOD  EQU   *                                                                
         MVI   0(R5),0                                                          
         CLC   0(3,R4),=C'POL'                                                  
         BCR   8,RE                                                             
         L     R6,APRDLST                                                       
GPC2     CLC   0(3,R6),0(R4)                                                    
         BCR   2,RE                                                             
         BE    GPC3                                                             
         CLI   0(R6),0                                                          
         BCR   8,RE                                                             
         LA    R6,4(R6)                                                         
         B     GPC2                                                             
GPC3     MVC   0(1,R5),3(R6)                                                    
GPCEXT   BR    RE                                                               
         SPACE 3                                                                
*                   GET PRODUCT MNEMONIC                                        
         SPACE 3                                                                
GETPRD   EQU   *                                                                
         CLI   0(R4),0                                                          
         BCR   8,RE                                                             
         L     R6,APRDLST                                                       
GP2      CLC   0(1,R4),3(R6)                                                    
         BE    GP3                                                              
         CLI   3(R6),0                                                          
         BCR   8,RE                                                             
         LA    R6,4(R6)                                                         
         B     GP2                                                              
GP3      MVC   0(3,R5),0(R6)                                                    
GPEXT    BR    RE                                                               
         SPACE 3                                                                
*                  CLEAR IOAREA                                                 
         SPACE 3                                                                
CLRIO    LA    RF,IOAREA                                                        
         LA    R0,8                                                             
         XC    0(250,RF),0(RF)                                                  
         LA    RF,250(RF)                                                       
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
ERRORX   DS    0H                  INSTEAD OF GENEROLS ERROR                    
         L     R4,ERRAREA          TO SET SYSTEM CODE                           
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVI   DMCB+20,2           SPOT SYSTEM CODE                             
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(X'FF',DMCB)                        
         B     EXIT                                                             
*                                                                               
       ++INCLUDE GENEROL                                                        
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
*  SPINVWK                                                                      
       ++INCLUDE SPINVWK                                                        
