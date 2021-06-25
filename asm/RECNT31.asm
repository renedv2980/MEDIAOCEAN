*          DATA SET RECNT31    AT LEVEL 221 AS OF 05/03/05                      
*PHASE T80231A                                                                  
*INCLUDE DAYUNPK                                                                
*INCLUDE UNTIME                                                                 
         TITLE 'T80231 - BUYCODE MAINTENANCE'                                   
***********************************************************************         
*                                                                     *         
*  RECNT31 -- BUYCODE MAINTENANCE                                     *         
*                                                                     *         
*  HISTORY:                                                           *         
*                                                                     *         
*  16MAR05 HQ  SKIP NON-HEADER PLAN RECORD ON DISPLAY                 *         
*  08OCT04 HQ  DELETED SHOULD BE X INSTEAD OF D                       *         
*  09MAR04 BU  ORIGINAL ENTRY                                         *         
*                                                                     *         
***********************************************************************         
T80231   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80231,RR=R5                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=H'4096'         4K                                           
         USING TWAWORK,R7                                                       
         LA    R4,LOCALTWA         SET A(SCRATCH SPACE)                         
         USING BCDDSECT,R4                                                      
*                                                                               
                                                                                
         CLI   PASSFLAG,C'S'       PREVIOUS PASS FROM THIS MODULE?              
         BE    MAIN0002            YES                                          
         CLI   PASSFLAG,C'U'       PREVIOUS PASS FROM THIS MODULE?              
         BE    MAIN0002            YES                                          
         CLI   PASSFLAG,C'E'       PREVIOUS PASS FROM THIS MODULE?              
         BE    MAIN0002            YES                                          
         XC    LINNUMS,LINNUMS     NO  - CLEAR LINE NUMBER SETTINGS             
         XC    TWALSTKY,TWALSTKY   CLEAR OUT LAST KEY                           
MAIN0002 EQU   *                                                                
*                                                                               
         LA    RF,BCDENDXH         SET A(END OF SCREEN)                         
         ST    RF,ASCRNEND                                                      
*                                                                               
         MVC   KEY+28(4),TWAKADDR  CONTRACT DISK ADDRESS                        
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                  RETRIEVE THE CONTRACT RECORD                 
         OC    LINNUMS,LINNUMS     ANY VALUE IN LINE NUMBERS?                   
         BZ    MAIN0005            NO  - NOTHING ON SCREEN                      
         OC    BCDLN#1,BCDLN#1     YES - ANYTHING IN FIRST LINE #               
         BZ    MAIN0010            NO  - SCREEN HAS BEEN CLEARED                
*                                     NEED TO RESET                             
MAIN0005 EQU   *                                                                
         CLC   TWAKADDR,BCDKADDR   NEW RECORD BEING DISPLAYED?                  
         BE    MAIN0020            NO  - PROCESSING SAME RECORD                 
MAIN0010 EQU   *                                                                
         MVI   PASSFLAG,C' '       YES - CLEAR FLAG                             
         MVI   CODEUPDT,C' '             CLEAR FLAG                             
         MVI   NEXTFLAG,0                CLEAR FLAG                             
*                                                                               
* FOUT AND CLEAR NON-ZERO LINES                                                 
*                                                                               
MAIN0020 EQU   *                                                                
         MVC   BCDKADDR,TWAKADDR   SAVE D/A OF ORDER DISPLAYED                  
         CLI   PASSFLAG,C'S'       SCREEN JUST FILLED?                          
         BE    MAIN0040            YES - SCAN FOR UPDATE                        
         CLI   PASSFLAG,C'E'       SCREEN RETURNED AN ERROR?                    
         BNE   MAIN0100            NO                                           
MAIN0040 EQU   *                                                                
         BAS   RE,BCUPDATE         YES - LOOK FOR UPDATES ON SCREEN             
         BNZ   UPDATERR            ERROR RETURN                                 
         TM    PROFILES+CNTBYCDB,CNTBYCDA                                       
         BNZ   MAIN0060            BUYCODE MANDATORY                            
*                                                                               
*   WHAT WE DO IF NOT MANDATORY:                                                
*        1.    UPDATE NO ERROR: SHOW MESSAGE, SAME SCREEN                       
*        2.    NO UPDATE:  SCROLL TO NEXT SCREEN                                
*                                                                               
         CLI   CODEUPDT,C'Y'       WAS A CODE UPDATED?                          
         BE    MAIN0060            YES - SHOW SAME SCREEN                       
*                                     THIS IS ALSO 'MANDATORY' PATH             
         MVI   PASSFLAG,C'U'       SET TO JUST UPDATED                          
         CLI   NEXTFLAG,0          SCREEN FULL RETURN?                          
         BE    MAIN0020            YES - GO BACK TO DISPLAY NEXT                
         B     MAIN0080            NO  - END DISPLAY LOOP                       
MAIN0060 EQU   *                                                                
         MVI   FULL,202            SET MSG: BUYCODES UPDATED                    
         MVI   PASSFLAG,C'U'       SET TO JUST UPDATED                          
         CLI   NEXTFLAG,0          SCREEN FULL RETURN?                          
         BE    MAINEXIT            YES                                          
MAIN0080 EQU   *                                                                
         MVI   FULL,204            SET MSG: ALL BUYCODES UPDATED                
         MVC   CONBACT(3),=C'   '  CLEAR BUY ACTION                             
         FOUT  CONBACTH                                                         
         CLI   NEXTFLAG,1          DATA FINISHED RETURN?                        
         BE    MAIN0400            YES                                          
         DC    H'0'                UNRECOGNIZED                                 
MAIN0100 EQU   *                                                                
*                                                                               
         MVI   STARTLIN,0          CLEAR STARTING LINE #                        
         LA    R2,CONBNUMH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    MAIN0120            YES                                          
*                                                                               
         LA    R3,INVINP           SET ERROR MESSAGE                            
         CLI   5(R2),0             TEST INPUT LENGTH                            
         BE    MAIN0120            NO INPUT                                     
         CLI   5(R2),3             TOO LONG: ERROR                              
         BH    ERROR                                                            
         SPACE                                                                  
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
         BCTR  R1,0                BACK UP ONE FOR EX COMMAND                   
         MVC   WORK(3),=3X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),CONBNUM                                                  
         CLC   WORK(3),=3X'F0'                                                  
         BL    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CONBNUM(0)                                                   
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR               ZERO IS INVALID BUYLINE NUMBER               
*                                                                               
         GOTO1 VPACK                                                            
         STC   R0,STARTLIN         STARTING LINE NUMBER                         
*                                                                               
MAIN0120 EQU   *                                                                
*                                                                               
         CLI   PASSFLAG,C'U'       SCREEN JUST UPDATED?                         
         BNE   MAIN0140            NO                                           
         BAS   RE,CLEARLNS         YES - CLEAR ALL LINES                        
MAIN0140 EQU   *                                                                
         MVC   CONBACT(3),=C'BCC'  SET TO UPDATE                                
         FOUT  CONBACTH                                                         
*                                                                               
         MVI   NEXTFLAG,0          SET UP A RETURN VALUE                        
         BAS   RE,BCDSPLAY         DISPLAY DATA                                 
         MVI   FULL,203            SET MSG: BUYCODES DISPLAYED                  
         CLI   NEXTFLAG,0          SCREEN FULL RETURN?                          
         BE    MAINEXIT            YES                                          
         CLI   NEXTFLAG,1          DATA FINISHED RETURN?                        
         BNE   MAIN0400            NO  - UNRECOGNIZED                           
         CLI   CTR+3,0             ANY LINES DISPLAYED?                         
         BH    MAIN0400            YES -                                        
         MVI   FULL,205            NO  - SET MSG: ALL CODES PROCESSED           
         MVC   CONBACT(3),=C'   '  CLEAR BUY ACTION                             
         FOUT  CONBACTH                                                         
         B     MAIN0400                                                         
MAIN0380 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED                                 
MAIN0400 EQU   *                                                                
         XC    TWALSTKY,TWALSTKY   CLEAR LAST KEY AT END                        
         FOUT  CONBNUMH,MYSPACES,8                                              
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+3(1),FULL      DISPLAY APPROPRIATE MESSAGE                  
         B     MAINEX20                                                         
MAINEXIT EQU   *                                                                
         XC    DMCB(24),DMCB                                                    
         NI    CONBACTH+6,X'BF'    NO CURSOR                                    
         MVC   CONBACT(3),=C'BCC'                                               
         MVC   DMCB+3(1),FULL      DISPLAY APPROPRIATE MESSAGE                  
         LA    R2,CONBNUMH                                                      
         MVC   8(4,R2),=C'NEXT'                                                 
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         OI    4(R2),X'20'         SET PREVIOUSLY VALID BIT                     
MAINEX20 EQU   *                                                                
         OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         GOTO1 VDISMSG,DMCB,,                                                   
         XIT1                                                                   
         EJECT                                                                  
UPDATERR LA    R3,NOBUYCOD         SET ERROR INDICATOR                          
         L     R2,DUB              SET A(ERROR)                                 
         MVI   PASSFLAG,C'E'       SET 'ERROR' FLAG                             
         B     ERROR                                                            
NOBUYCOD EQU   909                                                              
*                                                                               
**********************************************************************          
*  BCDSPLAY:  RETURNS BUYLINES AND DISPLAY THEM.  WHEN SCREEN IS     *          
*        FULL, NEXTFLAG WILL CONTAIN ZERO ON RETURN.  WHEN DATA IS   *          
*        EXHAUSTED, NEXTFLAG WILL CONTAIN 1 ON RETURN.               *          
*                                                                    *          
**********************************************************************          
BCDSPLAY NTR1                                                                   
*                                                                               
         XC    LINNUMS,LINNUMS     CLEAR LINE NUMBER SETTINGS                   
         LA    RF,LINNUMS          SET A(1ST LINE NUMBER SLOT)                  
         ST    RF,ALINNUMS                                                      
*                                                                               
         MVI   PASSFLAG,0          CLEAR CYCLING FLAG                           
*                                                                               
         XC    CTR,CTR             CLEAR COUNTER                                
*                                                                               
         OC    TWALSTKY,TWALSTKY   ANY 'LAST KEY' AVAILABLE?                    
         BZ    BDSP0020            NO  - START AT BEGINNING                     
         MVC   KEY(27),TWALSTKY                                                 
*                                  SET TO 'LAST KEY READ'                       
         GOTO1 VHIGH               RETRIEVE LAST KEY PROCESSED                  
         LA    R2,BCDMCD1H         SET A(1ST MOD CODE FIELD ON SCREEN)          
         B     BDSP0120            THEN GO READ SEQUENTIALLY                    
BDSP0020 EQU   *                                                                
         MVI   RBUYKTYP,X'0B'      BUILD BUY KEY                                
         MVC   RBUYKREP,REPALPHA   REP                                          
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
*                                                                               
         LA    R2,BCDMCD1H         SET A(1ST MOD CODE FIELD ON SCREEN)          
         USING BCDMCD1H,R2                                                      
*                                                                               
         CLC   CONBNUM(4),=C'NEXT'                                              
         BE    BDSP0080                                                         
BDSP0060 EQU   *                                                                
         B     BDSP0100                                                         
*                                                                               
*                                   NEXT PAGE REQUESTED                         
BDSP0080 EQU   *                                                                
         CLC   TWALSTKY(22),RBUYKEY                                             
*                                  SAME THRU CONTRACT #?                        
         BNE   BDSP0060            NO  -                                        
         MVC   RBUYKEY+22(5),TWALSTKY+22                                        
*                                  YES - RESET LAST FIVE BYTES                  
BDSP0100 MVC   KEY,RBUYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   CONBNUM(4),=C'NEXT' NEXT REQUEST?                                
         BNE   BDSP0140            NO - DON'T SKIP A REC                        
         CLC   KEY(22),KEYSAVE     NOTHING TO SHOW NEXT                         
         BE    BDSP0120                                                         
         MVI   NEXTFLAG,1          SET ERROR RETURN TO MAIN0400                 
         B     BDSP0900            EXIT                                         
*                                                                               
BDSP0120 EQU   *                                                                
         GOTO1 VSEQ                                                             
BDSP0140 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME THRU CONTRACT #?                        
         BE    BDSP0160            NO                                           
         MVI   NEXTFLAG,1          SET ERROR RETURN TO MAIN0400                 
         B     BDSP0900            EXIT                                         
BDSP0160 EQU   *                                                                
         TM    KEY+27,X'C0'        VOID?                                        
         BO    BDSP0120                                                         
K        USING RBUYREC,KEY                                                      
         CLC   K.RBUYKPLN,=X'FFFFFF' PLAN?                                      
         BE    BDSP0166                                                         
         CLC   K.RBUYKMLN,=X'FF'   SKIP NON-HEADER PLAN RECORD                  
         BE    BDSP0120                                                         
         DROP  K                                                                
BDSP0166 L     RF,CTR              INCREMENT COUNTER ON SCREEN                  
         LA    RF,1(RF)                                                         
         ST    RF,CTR              RESTORE COUNTER                              
*                                                                               
* GET BUY RECORD                                                                
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BDSP0120            YES - DON'T PROCESS                          
         CLI   RBUYCHGI+1,C'C'     BUY CANCELLED?                               
         BE    BDSP0120            YES - GO BACK FOR NEXT BUY                   
         CLI   RBUYCHGI,C'X'       BUY DELETED?                                 
         BE    BDSP0120            YES - GO BACK FOR NEXT BUY                   
         CLI   RBUYCHGI+1,C'X'     BUY DELETED?                                 
         BE    BDSP0120            YES - GO BACK FOR NEXT BUY                   
*                                                                               
*                                                                               
* DISPLAY LINE                                                                  
*                                                                               
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BE    BDSP0180                                                         
*                                                                               
         CLC   KEY+26(1),STARTLIN                                               
         BNE   BDSP0120            GO BACK FOR NEXT LINE                        
         MVI   STARTLIN,0          DON'T USE AGAIN                              
*                                                                               
BDSP0180 EQU   *                                                                
         L     RF,ALINNUMS         SET A(NEXT SLOT FOR LINE NUMBER)             
         MVC   0(2,RF),RBUYKMLN    SAVE MASTER + DETAIL LINE#                   
         LA    RF,2(RF)            BUMP TO NEXT SLOT                            
         ST    RF,ALINNUMS                                                      
*                                                                               
         MVI   PASSFLAG,C'S'       SET SCREEN TO 'JUST FILLED'                  
         EDIT  RBUYKLIN,(3,BCDLN#1)                                             
         FOUT  BCDLN#1H                                                         
         MVC   BCDMCD1(1),RBUYCHGI INSERT MOD CODE ON SCREEN                    
         FOUT  BCDMCD1H                                                         
         LA    R1,RBUYELEM                                                      
BDSP0200 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    BDSP0300            YES - ALL ELEMENTS SCANNED                   
         CLI   0(R1),X'21'         PROGRAM NAME ELEMENT?                        
         BE    BDSP0240            YES                                          
         CLI   0(R1),X'5F'         BUYCODE ELEMENT?                             
         BE    BDSP0280            YES                                          
BDSP0220 EQU   *                                                                
         ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         B     BDSP0200                                                         
BDSP0240 EQU   *                                                                
         ZIC   RF,1(R1)            GET ELEMENT LENGTH                           
         SH    RF,=H'3'            SUBTRACT CNTL + 1 FOR MOVE                   
         EX    RF,BDSP0260         MOVE BY LENGTH                               
         FOUT  BCDPRG1H                                                         
         B     BDSP0220            GO BACK FOR NEXT ELEMENT                     
BDSP0260 EQU   *                                                                
         MVC   BCDPRG1(0),2(R1)    MOVE PROG NAME BY LENGTH                     
BDSP0280 EQU   *                                                                
         MVC   BCDBCD1(3),2(R1)    INSERT BUY CODE INTO SCREEN                  
         OI    BCDBCD1H+4,X'20'    SET PREVIOUSLY VALID                         
         FOUT  BCDBCD1H                                                         
         B     BDSP0220            GO BACK FOR NEXT ELEMENT                     
BDSP0300 EQU   *                                                                
         LA    R3,RBUYELEM                                                      
BDSP0320 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BDSP0360            YES - ALL ELEMENTS SCANNED                   
         CLI   0(R3),X'02'         DAY/TIME ELEMENT?                            
         BE    BDSP0340            YES                                          
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     BDSP0320                                                         
BDSP0340 EQU   *                                                                
         LA    RF,3(R3)            SET A(RBUYDAYS)                              
         ST    RF,DMCB                                                          
         MVC   DMCB(1),2(R3)       INSERT START-END DAY INDICATOR               
         OI    DMCB,X'80'          SET REPPAK 11-BYTE O/P                       
         GOTO1 =V(DAYUNPK),DMCB,,BCDDAY1,RR=Y                                   
         FOUT  BCDDAY1H                                                         
         GOTO1 =V(UNTIME),DMCB,4(R3),BCDTIM1,RR=Y                               
         FOUT  BCDTIM1H                                                         
BDSP0360 EQU   *                                                                
         MVC   TWALSTKY,KEY                                                     
*                                                                               
BDSP0380 EQU   *                                                                
         LA    R2,BCDMCD2H-BCDMCD1H(R2)                                         
*                                  BUMP TO NEXT SCREEN LINE                     
         L     R3,ASCRNEND         END OF SCREEN REACHED?                       
         CR    R2,R3                                                            
         BNE   BDSP0120            NO  - GO BACK FOR ANOTHER RECORD             
         B     BDSP0900            YES - SCREEN FULL                            
BDSP0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*  BCUPDATE:  INTERROGATES EACH LINE ON WHICH DATA APPEARS, AND      *          
*        UPDATES THE BUYCODE INFORMATION INTO THE APPROPRIATE        *          
*        ORDER BUYLINE.  WILL RETURN AN ERROR IF FOUND.              *          
*                                                                    *          
**********************************************************************          
BCUPDATE NTR1                                                                   
         MVI   CODEUPDT,C'N'       SET 'NO UPDATE ON SCREEN'                    
         XC    LEADBCD,LEADBCD     CLEAR LEAD-IN CODE                           
         LA    R2,BCDMCD1H         SET A(FIRST LINE)                            
         USING BCDMCD1H,R2                                                      
*                                                                               
         LA    R3,LINNUMS          SET A(LINE NUMBER TABLE)                     
BUPD0020 EQU   *                                                                
         OC    0(2,R3),0(R3)       ANY ENTRY IN TABLE SLOT?                     
         BZ    BUPD0400            NO  - VALIDATION FINISHED                    
*                                     NOW OUTPUT THE NEW DATA                   
         TM    BCDBCD1H+4,X'20'    PREVIOUSLY VALID?                            
         BO    BUPD0060            YES - SAVE THIS AS LEAD-IN                   
         CLI   BCDBCD1H+5,0        ANY DATA IN FIELD?                           
         BE    BUPD0040            NO  - CHECK LEAD-IN                          
         CLI   BCDBCD1H+5,3        YES - DATA LENGTH 3 CHARS?                   
         BNE   BUPD0800            NO  - ERROR                                  
*                                                                               
*   CHECK FOR CODE EXISTENCE                                                    
*                                                                               
         XC    KEY,KEY             CLEAR KEY OUT                                
         MVI   KEY,X'4B'           SET KEY TYPE                                 
         MVC   KEY+21(2),REPALPHA  INSERT REP CODE                              
         MVI   KEY+23,1            SET RECORD TYPE = CODE                       
         MVC   KEY+24(3),BCDBCD1   INSERT CODE FROM LINE                        
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   BUPD0800            NO  - EXIT CC NOT ZERO: ERROR                
         MVC   LEADBCD,BCDBCD1     NO  - EXIT CC ZERO                           
         B     BUPD0050            BUMP TO NEXT LINE                            
BUPD0040 EQU   *                                                                
         TM    PROFILES+CNTBYCDB,CNTBYCDA                                       
         BZ    BUPD0050            BUYCODE NOT MANDATORY                        
*                                                                               
         OC    LEADBCD,LEADBCD     ANY LEAD-IN CODE?                            
         BZ    BUPD0800            NO  - MISSING CODE                           
         MVC   BCDBCD1,LEADBCD     YES - INSERT INTO SCREEN                     
         MVI   BCDBCD1H+5,3        SET FIELD LENGTH FOR UPDATE                  
         FOUT  BCDBCD1H                                                         
BUPD0050 EQU   *                                                                
         LA    R3,2(R3)            BUMP TO NEXT TABLE SLOT                      
         LA    R2,LINELEN(R2)      BUMP TO NEXT INPUT LINE                      
         B     BUPD0020            GO BACK FOR NEXT LINE                        
BUPD0060 EQU   *                                                                
         MVC   LEADBCD,BCDBCD1     SAVE CODE                                    
         B     BUPD0050            GO BACK FOR NEXT SLOT                        
BUPD0400 EQU   *                                                                
         BAS   RE,BCODWRIT         WRITE OUT BUYCODES                           
         SR    R0,R0               EXIT CC ZERO                                 
         B     BUPD0900            EXIT                                         
BUPD0800 EQU   *                                                                
         LA    RF,BCDBCD1H         SET ERROR LINE                               
         ST    RF,DUB                                                           
         LTR   RB,RB               SET CC NOT ZERO: ERROR                       
BUPD0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*  BCODWRIT:  CYCLES THROUGH LINES, AND UPDATES THE BUYS WITH        *          
*        THE NEW DATA.                                               *          
*                                                                    *          
**********************************************************************          
BCODWRIT NTR1                                                                   
         LA    R2,BCDMCD1H         SET A(FIRST LINE)                            
         USING BCDMCD1H,R2                                                      
*                                                                               
         LA    R3,LINNUMS          SET A(LINE NUMBER TABLE)                     
BCOD0020 EQU   *                                                                
         OC    0(2,R3),0(R3)       ANY ENTRY IN TABLE SLOT?                     
         BZ    BCOD0400            NO  - VALIDATION FINISHED                    
*                                                                               
         TM    BCDBCD1H+4,X'20'    PREVIOUSLY VALID SET?                        
         BO    BCOD0060            YES - DON'T HAVE TO UPDATE                   
         CLI   BCDBCD1H+5,0        ANY DATA IN FIELD?                           
         BE    BCOD0060            NO  - DON'T HAVE TO UPDATE                   
         MVI   CODEUPDT,C'Y'       SET 'CODE UPDATED'                           
*                                                                               
         XC    RBUYKEY,RBUYKEY     RETRIEVE RECORD                              
         MVI   RBUYKTYP,X'0B'      BUILD BUY KEY                                
         MVC   RBUYKREP,REPALPHA   REP                                          
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         MVC   RBUYKPLN,=X'FFFFFF'                                              
         MVC   RBUYKMLN(2),0(R3)   INSERT LINE NUMBER                           
         MVC   KEY,RBUYKEY                                                      
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY SHOULD BE ON FILE                        
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                  GET THE RECORD ITSELF                        
*                                                                               
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RBYSCDEL,R6                                                      
         MVI   RBYSCDCD,X'5F'      SET BUYCODE ELT TYPE                         
         MVI   RBYSCDLN,RBYSCDLQ   INSERT LENGTH                                
         MVC   RBYSCDBC,BCDBCD1    INSERT BUYLINE CODE FROM SCREEN              
         DROP  R6                                                               
*                                                                               
*   RETRIEVE ORIGINAL BUYCODE FOR PASSIVE KEY MAINTENANCE                       
*                                                                               
         XC    ORIGBYCD,ORIGBYCD   CLEAR ORIGINAL BUYCODE                       
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'5F'        RETRIEVE BUYCODE ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   BCOD0040            NO ELEMENT: NO CODE                          
         MVC   ORIGBYCD,RBYSCDBC-RBYSCDEL(R6)                                   
*                                  ELEMENT FOUND:  SAVE CODE                    
BCOD0040 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'5F',RBUYREC)                                    
*                                  DROP OLD X'5F' ELEMENT                       
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                  ADD  NEW X'5F' ELEMENT                       
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
*                                  REWRITE BUY RECORD                           
         MVC   NEWDADDR,KEY+28     SAVE BUY'S DISK ADDR                         
         BAS   RE,BUYCODE          ADD PASSIVE POINTER FOR RECORD               
*                                                                               
BCOD0060 EQU   *                                                                
         LA    R3,2(R3)            BUMP TO NEXT TABLE LINE                      
         LA    R2,LINELEN(R2)      BUMP TO NEXT SCREEN LINE                     
         B     BCOD0020            GO BACK FOR NEXT                             
BCOD0400 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*          DATA SET RECNT1E    AT LEVEL 206 AS OF 03/01/04                      
*                                                                               
*                                                                               
BUYCODE  NTR1                                                                   
         CLC   ORIGBYCD,=C'   '    NEED TO DELETE OLD CODE?                     
         BNH   BUYC0020            NO  - TREAT NEW CODE ONLY                    
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'9B01'     SET PASSIVE TYPE                             
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+13(3),ORIGBYCD  INSERT ORIGINAL BUYLINE CODE                 
         MVC   KEY+16(4),RCONKCON  INSERT CONTRACT NUMBER                       
         MVC   KEY+20(1),RBUYKLIN  INSERT BUYLINE NUMBER                        
         GOTO1 VHIGH               READ FOR KEY                                 
         CLC   KEY(21),KEYSAVE     KEY FOUND?                                   
         BNE   BUYC0020            NO  - TREAT AS IF NON-EXISTANT               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD               READ THE KEY FOR UPDATE                      
         OI    KEY+27,X'80'        SET KEY FOR DELETION                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
BUYC0020 EQU   *                                                                
*                                                                               
*   AT THIS POINT ADD OR REWRITE NEW CODES AS FOUND                             
*                                                                               
         LA    R6,RBUYELEM                                                      
BUYC0040 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BUYC0240            YES - NO X'5F' ELEMENT                       
         CLI   0(R6),X'5F'         BUYLINE CODE ELEMENT?                        
         BE    BUYC0060            YES - ADJUST PASSIVE KEYS                    
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     BUYC0040                                                         
BUYC0060 EQU   *                                                                
*                                                                               
         XC    KEY,KEY             CLEAR KEY FOR REBUILD                        
         MVC   KEY(2),=X'9B01'                                                  
         MVC   KEY+11(2),RCONKREP                                               
*                                  INSERT REP CODE                              
         MVC   KEY+13(3),RBYSCDBC-RBYSCDEL(R6)                                  
*                                  INSERT BUYLINE CODE                          
         MVC   KEY+16(4),RCONKCON  INSERT CONTRACT NUMBER                       
*                                                                               
         LA    RF,RBUYREC                                                       
         USING RBUYREC,RF                                                       
         MVC   KEY+20(1),RBUYKLIN  INSERT BUYLINE NUMBER                        
         MVC   KEY+21(3),=X'FFFFFF'                                             
*                                  INITIALIZE EARLIEST DATE                     
         XC    KEY+24(3),KEY+24    INITIALIZE LATEST DATE                       
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RBUYELEM                                                      
BUYC0080 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BUYC0180            YES - NO (MORE) X'03' ELEMENT(S)             
         CLI   0(R6),X'03'         EFFECTIVE DATE ELEMENT?                      
         BE    BUYC0120            YES - ADJUST PASSIVE KEYS                    
BUYC0100 EQU   *                                                                
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     BUYC0080                                                         
BUYC0120 EQU   *                                                                
         USING RBUYDTEL,R6                                                      
         CLC   RBUYDTST,KEY+21     EFF START DATE EARLIER?                      
         BH    BUYC0140            NO                                           
         MVC   KEY+21(3),RBUYDTST  YES - USE IT                                 
BUYC0140 EQU   *                                                                
         CLC   RBUYDTED,KEY+21     EFF END   DATE LATER?                        
         BNH   BUYC0160            NO                                           
         MVC   KEY+24(3),RBUYDTED  YES - USE IT                                 
BUYC0160 EQU   *                                                                
         B     BUYC0100            GO BACK FOR NEXT X'03' ELEMENT               
BUYC0180 DS    0H                                                               
*                                                                               
*   NEED TO DELETE ANY PRIOR KEY WHICH MAY HAVE DIFFERENT FLIGHT                
*                                                                               
         MVC   BYCDKEY,KEY         SAVE NEWLY BUILT KEY                         
         XC    KEY+21(6),KEY+21    CLEAR OUT FLIGHT DATES                       
         GOTO1 VHIGH               READ FOR NON-DELETED KEYS ONLY               
         CLC   KEY(21),KEYSAVE     KEY THROUGH BUYLINE# FOUND?                  
         BNE   BUYC0200            NO                                           
         MVI   UPDATE,C'Y'         READ WHOLE KEY FOR UPDATE                    
         GOTO1 VREAD                                                            
         OI    KEY+27,X'80'                                                     
*                                  SET KEY FOR DELETION                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
BUYC0200 EQU   *                                                                
         MVC   KEY(27),BYCDKEY     RESTORE NEWLY BUILT KEY                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS ALSO               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    BUYC0220            YES                                          
         MVC   KEY,BYCDKEY         NO  - ADD AS NEW KEY                         
         MVC   KEY+28(4),NEWDADDR                                               
         GOTO1 VADD                                                             
         B     BUYC0240                                                         
*                                                                               
BUYC0220 DS    0H                                                               
         MVI   UPDATE,C'Y'         READ KEY FOR UPDATE                          
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS ALSO               
         GOTO1 VREAD                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),NEWDADDR                                               
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE              UPDATE KEY                                   
         B     BUYC0240                                                         
BUYC0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLEARLNS NTR1                                                                   
         LA    R2,BCDMCD1H         SET A(1ST POS, FIRST LINE)                   
         LA    R0,SCRNLNS          SET LOOP CONTROL                             
         USING BCDMCD1H,R2                                                      
CLNS0020 EQU   *                                                                
         XC    BCDMCD1,BCDMCD1     CLEAR MOD CODE                               
         FOUT  BCDMCD1H                                                         
         XC    BCDLN#1,BCDLN#1     CLEAR LINE NUMBER                            
         FOUT  BCDLN#1H                                                         
         NI    BCDBCD1H+4,X'FF'-X'20'                                           
*                                  TURN OFF PREVIOUSLY VALID                    
         XC    BCDBCD1,BCDBCD1     CLEAR LINE CODE                              
         FOUT  BCDBCD1H                                                         
         XC    BCDDAY1,BCDDAY1     CLEAR DAY  FIELD                             
         FOUT  BCDDAY1H                                                         
         XC    BCDTIM1,BCDTIM1     CLEAR TIME FIELD                             
         FOUT  BCDTIM1H                                                         
         XC    BCDPRG1,BCDPRG1     CLEAR PROG FIELD                             
         FOUT  BCDPRG1H                                                         
         LA    R2,LINELEN(R2)      BUMP TO NEXT LINE                            
         BCT   R0,CLNS0020                                                      
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
CTR      DC    F'0'                                                             
ASCRNEND DS    A                                                                
ALINNUMS DS    A                                                                
SCRNLEN  EQU   BCDENDXH-BCDMCD1H   SCREEN LENGTH                                
LINELEN  EQU   BCDMCD2H-BCDMCD1H   LINE LENGTH                                  
SCRNLNS  EQU   SCRNLEN/LINELEN     NUM LINES / SCREEN                           
*                                                                               
STARTLIN DS    CL1                 1 BYTE BINARY STARTING LINE #                
LEADBCD  DS    CL3                 LEAD-IN BUYCODE FOR STUFF                    
NEWDADDR DS    XL4                                                              
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTCED                                                       
BCDDSECT DSECT                                                                  
LINNUMS  DS    XL24                LINE NUMBERS FROM SCREEN                     
*                                  11 LINES, 2 CHAR EACH                        
*                                  LINE 12 = DELIMITER                          
PASSFLAG DS    CL1                 LAST ACTION                                  
*                                  S = SCREEN JUST FILLED                       
*                                  U = SCREEN UPDATED                           
*                                  E = ERROR RETURNED                           
NEXTFLAG DS    CL1                 0  =  FULL SCREEN DISPLAYED                  
*                                  1  =  PARTIAL SCREEN DISPLAYED               
CODEUPDT DS    CL1                 N  =  NO UPDATE ON SCREEN                    
*                                  Y  =  UPDATE ON SCREEN                       
*                                                                               
BCDLSTKY DS    CL27                FIRST KEY ON SCREEN                          
BCDKADDR DS    F                   DISK ADDRESS OF RECORD                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'221RECNT31   05/03/05'                                      
         END                                                                    
