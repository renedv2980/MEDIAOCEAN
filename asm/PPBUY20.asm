*          DATA SET PPBUY20    AT LEVEL 088 AS OF 10/17/18                      
*PHASE T41120A                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY20 - CUSTOM COLUMN DATA DISPLAY/CHANGE'                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/12/16 LINE NUMBER FIX FOR PBU                                         
*                                                                               
* KWAN 07/15/15 SUPPORT MEDIA B (MOBILE), V (NATL VIDEO), W (LOCAL VID)         
*                                                                               
* KWAN 05/22/14 MEDIA L (SOCIAL)                                                
*                                                                               
* KWAN 08/18/11 PROTECT ADBUYER READ ONLY COLUMNS                               
*                                                                               
* KWAN 08/04/10 IDESK RECONCILIATION COLUMN CONTROL VIA IDK PROFILE             
*                                                                               
* SMYE 10/05/09 DISALLOW USE OF !FXRATE AS CODE IF FX PROFILE NOT SET           
*                                                                               
* KWAN 04/23/08 PROTECT FX COLUMN DATA                                          
*                                                                               
* BOBY 09/13/06 HANDLE STANDARD COLUMNS                                         
*                                                                               
* BOBY 03/13/06 CHANGE ONLY CC'S ENTERED THIS TIME.                             
*               DELETE CC ONLY IF DATA PORTION IS EMPTY                         
*                                                                               
* SMYE 07/06/05 FIX LOCK BUG                                                    
*                                                                               
* SMYE  01/04   START                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41120   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41120*,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     R8,=A(SUBROUTS)     VARIOUS ROUTINES                             
         A     R8,RELO                                                          
         USING SUBROUTS,R8                                                      
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         L     RF,=V(PPBROWSE)                                                  
         A     RF,RELO                                                          
         ST    RF,VPBROWSE                                                      
*                                                                               
         L     R7,AWRKREC                                                       
         USING WKT41120,R7                                                      
*                                                                               
         LA    RE,WKAREA           CLEAR WORKING AREAS                          
         LA    RF,WKAREAX          SHOULD BE LESS THAN 4095 BYTES               
         XCEFL                                                                  
         MVI   ACELTABX,X'FF'      INITIALIZE END OF TABLE MARKER               
*                                                                               
         MVC   DATADISP,=H'33'                                                  
*                                                                               
         MVI   IDESKCSW,0          IDESK COLUMN SWITCH                          
*                                                                               
         ZAP   LNCTR,=P'0'         INIT COUNTERS                                
         ZAP   FLDCTR,=P'0'                                                     
*                                                                               
         LA    R2,BUYCONH                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEED TO CLR HDR TITLE (MATRLS)               
         XC    8(6,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         LA    R6,BUYHDH                                                        
         USING PPEFD,R6            CUSTOM COLUMNNS LOWER SCREEN                 
*                                                                               
         LA    R2,CCLTRH                                                        
         ST    R2,TRADDR                                                        
         CLC   SVTRCODE,=C'CU'     CALLING FROM BUY00 OR BUY01?                 
         BE    AC30H                                                            
*                                                                               
         CLI   CCLTR,C'*'                                                       
         BNE   *+12                                                             
         LA    R3,NOTRERR                                                       
         B     ERROR                                                            
*                                                                               
         CLC   CCLTR,=C'RU'        RECALL?                                      
         BNE   *+12                                                             
         MVI   CCACT,RECALLCC      CUSTOM COL'S RECALL ACTION                   
         B     AC45                                                             
         CLC   CCLTR,=C'CU'        CHANGE?                                      
         BNE   *+12                                                             
         MVI   CCACT,CHANGECC      CUSTOM COL'S CHANGE ACTION                   
         B     AC45                                                             
         CLI   5(R2),1                                                          
         BNE   AC30                                                             
         CLI   8(R2),C'C'          CHANGE?                                      
         BNE   AC30                                                             
         MVI   CCACT,CHANGECC      CUSTOM COL'S CHANGE ACTION                   
         B     AC45                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NOTE: FOLLOWING PARAMETERS ARE GIVEN WHEN CALLED BY T41100                    
*                                                                               
* SVTRCODE - SAVED TRANSACTION CODE                                             
* BYTE4    - SAVED LENGTH OF DATE INPUT (MAX IS 8)                              
* DOUBLE   - SAVED DATE INPUT (IF ANY)                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AC30     CLC   SVTRCODE,=C'CU'     CHANGE BEFORE A RECALL?                      
         BNE   AC40                                                             
AC30H    MVC   CCLTR,=C'CU'        CODE MIGHT BE LOST FROM OVLAY CALLS          
         MVI   CCLTRH+5,2                                                       
         OI    CCLTRH+6,X'80'                                                   
         MVI   CCACT,CHANGECC      CUSTOM COL'S CHANGE ACTION                   
         OC    DOUBLE,DOUBLE       ANY DATE SAVED FROM T41100 CALL?             
         BZ    AC45                                                             
         CLI   BYTE4,0                                                          
         BE    AC45                                                             
         MVC   CCLDTEH+5(1),BYTE4  GET LENGTH                                   
         ZIC   RE,BYTE4                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CCLDTE(0),DOUBLE    GET DATE DATE                                
         OI    CCLDTEH+6,X'80'                                                  
*                                                                               
         OI    CHGSW,NEWCUSCR      FIRST TR CODE IS CU                          
         B     AC45                GO VALIDATE INS DATE AGAIN                   
*                                                                               
AC40     DS    0H                                                               
         XC    TRADDR,TRADDR       NXTTR ROUTINE WILL FIGURE IT OUT             
         BAS   R9,NXTTR                                                         
         BNZ   AC45                                                             
         LA    R3,NOTRERR          NO VALID TR CODE ENTERED ERR MSG             
         B     ERROR                                                            
*                                                                               
AC45     CLI   SVSCRN,X'EF'        CUSTOM COLUMNS SCREEN?                       
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*                                                                               
         LA    R2,CCLTRH                                                        
         CLC   CCLTR,=C'DL'                                                     
         BNE   *+12                                                             
         LA    R3,CCDELERR         DELETING A DELETED BUY ERR MSG               
         B     ERRORXT                                                          
*                                                                               
         CLI   CCLTR,C'B'                                                       
         BNE   *+12                                                             
         LA    R3,NBCCERR          NO BUYS ON CUSTOM COLUMNS SCREEN             
         B     ERRORXT                                                          
*                                                                               
         LA    R2,CCLDTEH          POINT TO DATE FLD                            
         TM    4(R2),X'80'         FIELD INPUT THIS TIME?                       
         BZ    *+8                                                              
         OI    CHGSW,DATECHGD      DATE FLD CHANGED, FORCE TO REDISPLAY         
*                                                                               
         TM    CHGSW,DATECHGD      DATE FLD CHANGED?                            
         BZ    AC45M                                                            
         TM    4(R2),X'20'         DATE FLD PREVIOUSLY VALIDATED?               
         BZ    AC45M                                                            
         NI    CHGSW,X'FF'-DATECHGD                                             
*                                                                               
AC45M    CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,INVDTERR         INVALID DATE FORMAT ERR MSG                  
         B     ERROR                                                            
*                                                                               
         BAS   R9,EDTINS           EDIT INSERTION DATE                          
*                                                                               
         BAS   RE,NXTINS           INSERTION FOUND?                             
         BE    *+16                                                             
         LA    R2,BUYPBH                                                        
         LA    R3,NOINSERR         NO INSERTIONS ON FILE ERR MSG                
         B     ERROR                                                            
*                                                                               
         CLI   BYTE2,C'D'          DELETED INSERTION? PASSED BY NXTINS          
         BNE   AC60                                                             
         CLI   CCACT,RECALLCC      RECALL?                                      
         BE    AC47                OKAY TO DISP CUSTOM COLUMNS DATA             
         LA    R2,CCLDTEH                                                       
         LA    R3,INDELNCC                                                      
         B     ERRORXT                                                          
*                                                                               
AC47     MVC   CCLTR,=C'D '        INDICATE DELETED BUY                         
         MVI   CCLTRH+5,2                                                       
         OI    CCLTRH+6,X'80'                                                   
*                                                                               
AC60     DS    0H                  CHECK FOR OTHER VALIDATIONS                  
*                                                                               
AC70     LA    RE,REC+33                                                        
         CLI   0(RE),X'20'         BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
*                                                                               
AC80X    DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   CCACT,RECALLCC      CUSTOM COL'S RECALL?                         
         BNE   AC90                NO                                           
         BRAS  RE,CCDISPLY         DISPLAY CUSTOM COLUMNS                       
         B     ALLDONE                                                          
*                                                                               
AC90     CLI   CCACT,CHANGECC      CUSTOM COL'S CHANGE?                         
         BNE   DEADEND                                                          
         TM    CHGSW,NEWCUSCR      FIRST TR CODE IS CU?                         
         BO    DEADEND             YES - INVALID TRANS CODE                     
         TM    CHGSW,DATECHGD      DATE FLD CHANGED?                            
         BO    DEADEND4            YES - MUST BE RECALLED                       
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      LOGIC FOR CHANGE BELOW                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         BRAS  RE,CCDUPCK          CHECK FOR DUPLICATE CODES                    
         BNE   ERRORXT             DUPLICATE DETECTED                           
         BRAS  RE,CCEDIT           EDIT CUSTOM COLUMNS                          
         BNE   ERRORXT             ERROR DETECTED                               
         BRAS  RE,CCCHANGE         UPDATE BUYREC (REC)                          
         BNE   ERRORXT             ERROR DETECTED                               
         BRAS  RE,CCUPDTE          WRITE CHANGED BUY AND CUST-COL RECS          
         BNE   ERRORXT             ERROR DETECTED                               
         XC    CCENDEL,CCENDEL     START NEXT DISPLAY FROM BEGINNING            
*NOP*    BRAS  RE,CCDISPLY         DISPLAY "NEW" CUSTOM COLUMNS                 
         B     ALLDONE                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      LOGIC FOR CHANGE ABOVE                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
DEADEND  DS    0H                  NO OTHER DEFINED ACTIONS                     
         LA    R2,CCLTRH                                                        
         LA    R3,INVTRERR         INVALID TR CODE ERR MSG                      
         B     ERROR                                                            
*                                                                               
DEADEND4 DS    0H                                                               
         LA    R2,CCLTRH                                                        
         LA    R3,CCNEEDR          RECALL REQUIRED WHEN DATE CHANGED            
         B     ERRORXT                                                          
*                                                                               
ALLDONE  DS    0H                                                               
         CLC   CCLTR,=C'D '        DELETED BUY BEING DISPLAYED?                 
         BNE   ALLD20                                                           
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(L'DELMSG),DELMSG                                          
         OI    BUYMSGH+6,X'80'                                                  
         B     ALLD80                                                           
*                                                                               
ALLD20   DS    0H                                                               
         CLC   CCLTR,=C'RU'                                                     
         BNE   ALLD40                                                           
         OC    SVSCRCC,SVSCRCC     ANYTHING DISPLAYED ?                         
         BNZ   ALLD24              YES                                          
         BRAS  RE,DSPCLR           CLEAR LOWER SCREEN                           
         B     ALLD40                                                           
ALLD24   XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(L'CCENDM),CCENDM    "END OF CODES"                        
         OC    CCENDEL,CCENDEL     MORE TO DISPLAY ?                            
         BZ    ALLD28              NO                                           
         XC    BUYMSG,BUYMSG       YES                                          
         MVC   BUYMSG(L'CCMORM),CCMORM    "... ENTER FOR NEXT"                  
ALLD28   OI    CCLTRH+6,X'81'      SET MODIFIED AND XMIT                        
         OI    BUYMSGH+6,X'80'                                                  
         B     ALLD80                                                           
*                                                                               
ALLD40   DS    0H                                                               
         XC    CCLTR,CCLTR         BLANK OUT TRANSACTION CODE                   
         MVI   CCLTR,C'*'                                                       
         OI    CCLTRH+6,X'80'                                                   
*                                                                               
ALLD60   XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(L'CMPMSG),CMPMSG                                          
         OI    BUYMSGH+6,X'80'                                                  
*                                                                               
ALLD80   MVI   ERRAREA,C'K'        FAKE ERROR TO SEND THIS MSG                  
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'** ACTION COMPLETED **'                                        
DELMSG   DC    C'** DELETED BUY WITH CUSTOM COLUMNS DISPLAYED **'               
CCMORM   DC    C'** CODES DISPLAYED - CHANGE OR HIT ENTER FOR NEXT'             
CCENDM   DC    C'** END OF CODES - CHANGE OR ADD OR HIT ENTER FOR FIRSTX        
                '                                                               
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
SUBROUTS DS    0D                  R8 IS BASE REGISTER HERE           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1  ,                   XIT1 AT DMCHECK                              
         GOTOR VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR1  ,                   XIT1 AT DMCHECK                              
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTOR VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* DATA MANAGER ERRORS AND EXIT                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT1                                                                   
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         J     ERROR                                                            
*                                                                               
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTOR VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         ZAP   DUB,LNCTR                                                        
         CVB   RF,DUB                                                           
         STC   RF,HALF2                                                         
*                                                                               
         ZAP   DUB,FLDCTR                                                       
         CVB   RF,DUB                                                           
         STC   RF,HALF2+1                                                       
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
*                                                                               
ERRORXT  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         MVI   ERRAREA,X'FF'                                                    
         GOTOR (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVESCR  NTR1                                                                   
         LA    RE,SVSCREEN                                                      
         LA    RF,SVSCREEQ                                                      
         XCEFL                                                                  
*                                                                               
         LA    R2,CCLCOD1H         POINT TO 1ST FLD ON CUST COL LIST            
         LA    R3,SVSCREEN         POINT TO TABLE                               
         LA    RF,10               TEN LINES TO BE SAVED                        
*                                                                               
SAVESC30 MVC   00(12,R3),8(R2)     CODE                                         
         BAS   R9,BUMPFLD                                                       
         MVC   12(60,R3),8(R2)     DATA                                         
         BAS   R9,BUMPFLD                                                       
         LA    R3,SVSCRLQ(R3)                                                   
         BCT   RF,SAVESC30                                                      
*                                                                               
SAVESCRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOADSCR  NTR1                                                                   
*                                                                               
         LA    R2,CCLCOD1H         POINT TO 1ST FLD ON CUST COL LIST            
         LA    R3,SVSCREEN         POINT TO TABLE                               
         LA    RF,10               TEN LINES TO BE LOADED                       
*                                                                               
LOADSC30 MVC   08(12,R2),00(R3)    CODE                                         
         BAS   R9,BUMPFLD                                                       
         MVC   08(60,R2),12(R3)    CHARGE                                       
         BAS   R9,BUMPFLD                                                       
         LA    R3,SVSCRLQ(R3)                                                   
         BCT   RF,LOADSC30                                                      
*                                                                               
LOADSCRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRREC   NTR1                      CLEAR END OF RECORD                          
         MVC   HALF,REC+25         RECORD LENGTH                                
         LA    RE,REC                                                           
         AH    RE,HALF             END OF REC (FROM RECORD LENGTH)              
         LHI   RF,4000             MAX RECORD LENGTH                            
         SH    RF,HALF             NUMBER OF BYTES TO BE CLEARED                
         XCEFL                                                                  
CLRRECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PEOREC   NTR1                                                                   
         LA    R5,REC+33           FIRST ELEM OF BUY REC                        
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NEXTEL           R5 SHOULD POINT TO END OF REC                
         XIT1  REGS=(R5)                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUMPFLDS DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         TM    1(R2),X'02'         EXTENDED FIELD HEADER ?                      
         BZ    *+8                 NO                                           
         AHI   R2,8                                                             
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    R9                                                               
*                                                                               
BUMPFLD2 DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         TM    1(R2),X'02'         EXTENDED FIELD HEADER ?                      
         BZ    *+8                 NO                                           
         AHI   R2,8                                                             
         AR    R2,R0                                                            
BUMPFLD  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         TM    1(R2),X'02'         EXTENDED FIELD HEADER ?                      
         BZ    *+8                 NO                                           
         AHI   R2,8                                                             
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTR    DS    0H                                                               
         GOTOR VNXTTR                                                           
         CLI   ERRAREA,0                                                        
         BNE   TESTERR                                                          
         XC    INSDA,INSDA                                                      
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         XC    BINSDT,BINSDT                                                    
         MVI   BSUBLN,0                                                         
         L     R2,TRADDR           GET NEW TR ADDR                              
         MVC   TRCODE,8(R2)                                                     
         LTR   R2,R2                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
TESTERR  CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTINS   NTR1                                                                   
         MVI   BYTE2,0             FLAG FOR INDICATING DELETED BUYS             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         XC    KEY,KEY             INIT KEY BUILD AREA                          
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),BUYMD                                                 
         MVI   KEY+03,X'20'        BUY RECORD ID CODE                           
         MVC   KEY+04(03),BUYCL                                                 
         MVC   KEY+07(03),BUYPR                                                 
         MVC   KEY+10(06),BPUB                                                  
         MVC   KEY+16(03),BINSDT                                                
         MVC   KEY+19(02),BEST                                                  
         MVC   KEY+24(01),BSUBLN                                                
         CLI   KEY+24,0                                                         
         BNE   *+8                                                              
         MVI   KEY+24,1                                                         
*                                                                               
         CLI   MADSW,C'Y'          SCRIPT UPLOAD?                               
         JNE   NXTINS1                                                          
         L     RE,ATHISTMP         POINT TO UPLOAD OBJECT                       
         LA    RE,2(RE)            POINT PASS LENGTH                            
         USING PINSD,RE                                                         
         CLC   =C'DEL',8(RE)       DELETE OBJECT?                               
         JE    *+10                                                             
         MVC   KEY+24(01),PINSLINE USE LINE NUMBER FROM UPLOAD OBJECT           
         DROP  RE                                                               
*                                                                               
NXTINS1  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NXTINSX                                                          
         CLI   KEY+25,X'FF'                                                     
         BNE   NXTINS2+4                                                        
         LTR   RE,RE                                                            
         B     NXTINSX                                                          
NXTINS2  BAS   RE,SEQ                                                           
         CLI   KEY+25,X'FF'                                                     
         BE    NXTINS2                                                          
         CLC   KEY(16),KEYSAVE     TEST SAME THRU PUB                           
         BNE   NXTINSX                                                          
*                                                                               
         CLC   KEY+19(2),BEST      TEST RIGHT EST                               
         BNE   NXTINS2                                                          
         OC    KEY+21(3),KEY+21    TEST ACTIVE                                  
         BNZ   NXTINS2             NO                                           
*                                                                               
         TM    KEY+25,X'80'        DELETED?                                     
         BZ    *+8                                                              
         MVI   BYTE2,C'D'          YES, DELETED                                 
         BAS   RE,GETREC                                                        
*                                                                               
NXTINS2X DS    0H                  SAVE DATA IN SVINS LIST                      
*                                                                               
NXTINS3  LA    R0,DUMEL                                                         
         C     R0,TRADDR           TEST FOR DUMMY LINE                          
         BE    NXTINSX                                                          
         LA    R1,SVINS            FIND A SLOT                                  
         OC    0(6,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,6(R1)                                                         
         B     *-14                                                             
         L     R0,TRADDR           GET REL TWA ADDR                             
         SR    R0,RA                                                            
         STH   R0,0(R1)                                                         
NXTINS4  DS    0H                                                               
         MVC   2(4,R1),KEY+27      SAVE DISK ADDRESS                            
         CR    R2,R2               SET CC                                       
*                                                                               
NXTINSX  DS    0H                                                               
         LA    R1,1                PRESERVE CC                                  
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         NI    DMINBTS,X'F7'       RESET DELETES                                
*                                                                               
         LTR   R1,R1                                                            
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTINS   DS    0H                                                               
         GOTOR VEDTINS,DMCB,(RC),(RA)                                           
         CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*NTRY - R2 POINTING TO 60-BYTE "LINE" HEADER                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETLNTH  NTR1                      GET LENGTH OF DATA ON LINE                   
         LA    R2,8(R2)                                                         
         LA    RE,59(R2)           POINT RE TO LINE END                         
GETLLUP  CLI   0(RE),C' '          ANYTHING THERE ?                             
         BH    GETLBAK             YES - GO CALCULATE LENGTH                    
         AHI   RE,-1               MOVE TO LEFT ON LINE                         
         CR    RE,R2               RE GREATER OR EQUAL ?                        
         BNL   GETLLUP             YES                                          
         DC    H'0'                NOTHING FOUND ON LINE                        
GETLBAK  DS    0H                                                               
         AHI   RE,1                                                             
         SR    RE,R2               LENGTH OF DATA ON LINE TO RE                 
         STC   RE,DTALNTH          SAVE LENGTH                                  
GETLNTHX DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*NTRY - R2 POINTING TO 60-BYTE "LINE" HEADER                                    
*NTRY - DTALNTH=LENGTH OF DATA ON "LINE" (FROM ABOVE ROUTINE)                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETDEC   NTR1                      GET NUMBER OF DECIMAL PLACES                 
         LA    RF,0                                                             
         LA    RE,8(R2)            POINT RE TO DATA START                       
GETDLUP  CLI   0(RE),C' '          ANYTHING THERE ?                             
         BNH   GETDBAK             NO - FINISH                                  
         CLI   0(RE),C'.'          DECIMAL POINT ?                              
         BE    GETDBAK             YES - FINISH                                 
         AHI   RE,1                MOVE TO RIGHT ON LINE                        
         AHI   RF,1                ADD TO COUNT OF CHAR'S BEFORE "END"          
         B     GETDLUP             TEST NEXT                                    
GETDBAK  DS    0H                                                               
         ZIC   RE,DTALNTH          LENGTH OF DATA ON THE LINE                   
         AHI   RF,1                ADD TO COUNT FOR DECIMAL POINT               
         SR    RE,RF               RE HAS COUNT AFTER DECIMAL POINT             
         LTR   RE,RE               POSITVE ?                                    
         BNM   *+8                 YES                                          
         LA    RE,0                NO DECIMAL PLACES                            
         STC   RE,SCOLDECS         SAVE COUNT                                   
GETDECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
CHANGECC EQU   01                  CHANGE CUSTON COLUMNS ACTION                 
RECALLCC EQU   02                  RECALL CUSTOM COLUMNS ACTION                 
*                                                                               
         LTORG                                                                  
         TITLE 'PPBUY20 - CUSTOM COLUMN DATA DISPLAY LOGIC'                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCDISPLY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY           SAVE BUY KEY                                 
*                                                                               
         XC    SVIDKPRF,SVIDKPRF   IDESK CONTROL PROFILE VALUES                 
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL                                                 
         CLI   SVCLTOFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFC                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,VDATAMGR                         
*                                                                               
         LA    R2,CCLCOD1H         POINT R2 TO 1ST CUST COL SCREEN FLD          
         LA    R5,REC+33                                                        
         USING BYCCELD,R5          BUY CUSTOM COLUMN ELEMENT                    
*                                                                               
         MVI   ELCODE,X'CC'                                                     
         XC    SVSCRCC(SVSCRCCQ),SVSCRCC    CLEAR SAVE SCREEN DATA              
         LA    RE,SVSCRCC                                                       
         ST    RE,SVCCREG          SAVE (A)SAVE SCREEN DATA ENTRY               
*                                                                               
         OC    CCENDEL,CCENDEL     BYCCSQN OF LAST ELEM DISPLAYED               
         BZ    CCDIS15             START FROM BEGINNING                         
*                                                                               
         CLI   CCLCOD2,C' '        ONLY 1 LINE ON SCREEN ?                      
         BH    CCDIS10             NO - START AT LAST ELEM DISPLAYED            
*                                                                               
         XC    CCENDEL,CCENDEL     CLEAR                                        
*                                                                               
         B     CCDIS15             START FROM BEGINNING                         
*                                                                               
CCDIS10  DS    0H                  FIND FIRST ELEMENT TO DISPLAY                
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CCDISX              DONE                                         
*                                                                               
         CLC   CCENDEL,BYCCSQN     START DISPLAY HERE ?                         
         BNE   CCDIS10             NO - TEST NEXT                               
*                                                                               
         BRAS  RE,DSPCLR           CLEAR LOWER SCREEN                           
*                                                                               
         B     CCDIS20G            START                                        
*                                                                               
CCDIS15  DS    0H                                                               
*                                                                               
         BRAS  RE,DSPCLR           CLEAR LOWER SCREEN                           
*                                                                               
CCDIS20  DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CCDISX              DONE                                         
*                                                                               
CCDIS20G DS    0H                  GET CUSTOM COL RECORD                        
*                                                                               
         NI    1(R2),X'FF'-X'20'   INIT PROTECTION                              
*                                                                               
         CLC   BYCCSQN,=X'2000'    IF LESS THAN THIS                            
         BL    CCDISCC0               NORMAL CUSTOM COLUMN                      
*                                                                               
*                                  ELSE STANDARD COLUMN AND ON GENFIL           
*                                                                               
*        READ PASSIVE POINTER VIA INTERNAL SQN NUMBER                           
*                                                                               
         XC    GKEY,GKEY                                                        
         LA    R4,GKEY                                                          
         USING GCOLPKEY,R4                                                      
*                                                                               
         MVC   GCOLPRID,=AL3(GCOLPRIQ)  SET GENDIR PASSIVE KEY ID               
         MVI   GCOLPMED,C'A'       MEDIA ALWAYS A FOR CUST COL'S                
         MVI   GCOLPRCD,PCOLPRCQ   RECORD CODE FOR PASSIVE KEY                  
         MVC   GCOLPSQN,BYCCSQN    SEQUENCE NUMBER                              
         XC    GCOLPSQN,=X'FFFF'   COMPLEMENT SEQUENCE NUMBER                   
*                                                                               
         MVC   GKEYSAVE,GKEY                                                    
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'GENDIR',          X        
               GKEY,GKEY,(TERMNAL,0)                                            
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         CLC   GCOLPKEY,GKEYSAVE                                                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R4,CCREC            100-BYTE AREA FOR CUSTOM COL REC             
         ST    R4,AREC                                                          
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'GENFILE',         X        
               GKEY+36,AREC,(TERMNAL,DMWORK)                                    
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         MVC   8(12,R2),GCOLKCOD   COLUMN CODE                                  
*                                                                               
         CLC   BYCCSQN,=X'200F'                                                 
         BNE   *+12                                                             
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         MVI   5(R2),7             LENGTH FOR !FXRATE                           
*                                                                               
         NI    IDESKCSW,X'FF'-IDKRECNQ                                          
         TM    REC+(PBDSTAT2-PBUYREC),X'20'                                     
         BZ    CCDIS20M                                                         
         CLI   SVIDKPRF+00,C'Y'                                                 
         BNE   CCDIS20M                                                         
         CLC   BYCCSQN,=X'2010'                                                 
         BNE   CCDIS20M                                                         
         OI    1(R2),X'20'         PROTECT FIELD FOR IDESK RECNCLN              
         MVI   5(R2),11            LENGTH FOR !DESKRECONC                       
         OI    IDESKCSW,IDKRECNQ                                                
*                                                                               
CCDIS20M DS    0H                                                               
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R4,GCOLFRST         POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
         B     CCDISCC1                                                         
*                                                                               
CCDISCC0 DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PCOLRECD,KEY                                                     
*                                                                               
         MVC   PCOLPAGY,AGYALPHA AGENCY                                         
         MVI   PCOLPMED,C'A'     MEDIA ALWAYS A FOR CUST COL'S                  
         MVI   PCOLPRCD,PCOLPRCQ RECORD CODE FOR PASSIVE KEY                    
         MVC   PCOLPSQN,BYCCSQN  SEQUENCE NUMBER                                
         XC    PCOLPSQN,=X'FFFF'    COMPLEMENT SEQUENCE NUMBER                  
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'PCOLPKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R4,CCREC            100-BYTE AREA FOR CUSTOM COL REC             
         ST    R4,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         USING PCOLRECD,R4                                                      
*                                                                               
         MVC   8(12,R2),PCOLKCOD   COLUMN CODE                                  
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R4,PCOLFRST         POINT TO FIRST ELM IN RECORD                 
*                                                                               
CCDISCC1 DS    0H                                                               
*                                                                               
         USING PCOLELEM,R4         ESTABLISH TRANSLATE ELEMENT                  
*                                                                               
         L     RE,SVCCREG          POINT TO SVSCRCC TABLE                       
         MVC   0(L'BYCCSQN,RE),BYCCSQN    SAVE BUY CC ELEM "KEY"                
         LA    RE,L'BYCCSQN(RE)                                                 
         ST    RE,SVCCREG          NEXT "SAVE" AREA                             
         MVC   CCENDEL,BYCCSQN     LAST ELEMENT DISPLAYED                       
*                                                                               
*        CHECK TO PROTECT CODE FIELD                                            
*                                                                               
         CLI   PCOLRDWR,C'Y'       ADBUYER READ ONLY COLUMN?                    
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD - READ ONLY                    
*                                                                               
         BAS   R9,BUMPFLD          NEXT SCREEN FIELD - DATA                     
*                                                                               
         NI    1(R2),X'FF'-X'20'   INIT PROTECTION                              
*                                                                               
         TM    IDESKCSW,IDKRECNQ                                                
         BZ    *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD FOR IDESK RECNCLN              
*                                                                               
         CLI   PCOLRDWR,C'Y'       ADBUYER READ ONLY COLUMN?                    
         BNE   *+8                                                              
         OI    1(R2),X'20'         PROTECT FIELD - READ ONLY                    
*                                                                               
         CLI   PCOLTYP,C'D'        DATE ?                                       
         BE    CCDIS30             YES                                          
         CLI   PCOLTYP,C'P'        PERIOD ?                                     
         BE    CCDIS40             YES                                          
         CLI   PCOLTYP,C'T'        TEXT ?                                       
         BNE   CCDIS50             NO - MUST BE 1 OF 3 NUMERIC "TYPES"          
*                                                                               
*                    EXECUTED MOVE OF BYCCDATA TO SCREEN FOR TEXT DATA          
         ZICM  R1,BYCCLEN          TOTAL ELEMENT LENGTH                         
         LA    RF,BYCCHDRL         HEADER LENGTH (OVERHEAD)                     
         SR    R1,RF               R1 HAS LENGTH OF CUST COL DATA               
         BCTR  R1,0                PREP FOR EXECUTED MOVE                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BYCCDATA                                                 
         B     CCDISBK             NEXT SCREEN FIELD                            
*                                                                               
CCDIS30  DS    0H                  OUTPUT A SINGLE DATE                         
         GOTOR VDATCON,DMCB,(X'03',BYCCDATA),(5,8(R2))                          
         B     CCDISBK             NEXT SCREEN FIELD                            
*                                                                               
CCDIS40  DS    0H                  OUTPUT A DATE RANGE (PERIOD)                 
         GOTOR VDATCON,DMCB,(X'13',BYCCDATA),(5,8(R2))                          
         B     CCDISBK             NEXT SCREEN FIELD                            
*                                                                               
CCDIS50  DS    0H                  OUTPUT A NUMERIC FIELD (N, $, OR %)          
         XC    WORK,WORK           PREP FOR EDITOR USE                          
         LA    R3,WORK                                                          
         USING EBLOCKD,R3                                                       
         LA    RF,BYCCDATA         SET UP EDITOR INPUT BLOCK                    
         ST    RF,EBAIN            INPUT ADDRESS                                
         MVI   EBLIN,8             INPUT LENGTH (ALWAYS PL8)                    
         MVI   EBTIN,C'P'          INPUT TYPE (PACKED)                          
         LA    RF,8(R2)            SET UP EDITOR OUTPUT BLOCK                   
         ST    RF,EBAOUT           OUTPUT ADDRESS                               
         MVC   EBDECS,PCOLDECS     NO OF DECIMAL PLACES                         
         MVI   EBALIGN,C'L'        LEFT ALIGN                                   
         MVC   EBLOUT,PCOLMLEN     OUTPUT LENGTH                                
         OI    EBOPT,EBOQCEY       COMMAS=YES                                   
         MVI   EBFLOAT,C'-'        MINUS =YES                                   
*                                                                               
         GOTOR VEDITOR,DMCB,EBLOCK                                              
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
CCDISBK  DS    0H                                                               
         CLC   BYCCSQN,=X'200F'                                                 
         BNE   CCDISBK2                                                         
         OI    1(R2),X'20'         PROTECT FIELD FOR FX                         
         LA    RE,8(R2)            POINT TO DATA                                
         SR    RF,RF               CHARACTER COUNT                              
         CLI   0(RE),C' '          END OF INPUT?                                
         BNH   *+16                                                             
         AHI   RF,1                                                             
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         STC   RF,5(R2)            LENGTH OF FX RATE                            
CCDISBK2 OI    6(R2),X'80'         XMIT                                         
         BAS   R9,BUMPFLD          NEXT SCREEN FIELD - CODE                     
         LA    RE,CCLCODAH         LAST DATA LINE ON SCREEN                     
         CR    R2,RE               ROOM LEFT ?                                  
         BNH   CCDIS20             YES - NEXT CUSTOM COLUMN ELEMENT             
*                                  DONE WITH THIS SCREEN                        
         DROP  R5                                                               
*                                                                               
CCDISX   DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY AND                              
*                                        SET OFF "MODIFIED" BIT WHICH           
         NI    CCLDTEH+1,X'FF'-X'01'     MAY HAVE BEEN SET IN RM ACTION         
         OI    CCLDTEH+6,X'80'           XMIT                                   
*                                                                               
         CLI   CCLCODA,C' '        ANYTHING IN LAST DATA LINE ?                 
         BH    CCDISXX             YES                                          
         XC    CCENDEL,CCENDEL     NO - NO NEED TO SCROLL                       
CCDISXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
SVCCREG  DS    F                          NEXT SVSCRCC FIELD                    
*                                                                               
         LTORG                                                                  
   TITLE 'PPBUY20 - CUSTOM COLUMN CODE DUPLICATE TEST (CCDUPCK)'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PUT ALL SCREEN "CODES" (MAX 10) INTO A TABLE         
CCDUPCK  NTR1  BASE=*,LABEL=*                                                   
         XC    CCCODES,CCCODES     CLEAR 10 X CL12 TABLE                        
         MVI   CCCODES,X'FF'       MARK END OF TABLE                            
         LA    R2,CCLCOD1H         R2-> 1ST CUST COL "CODE" HDR                 
         LA    RF,CCLTEXTH         RF-> END OF SCREEN INPUT LINES               
         MVI   CCCODES,X'FF'       MARK "END" OF STORED CODES                   
CDUPTOP  DS    0H                                                               
         CR    R2,RF               END OF SCREEN INPUT ?                        
         BNL   CDUPOKX             YES - NO DUPLICATES FOUND                    
         CLI   8(R2),C' '          ANYTHING IN CODE FIELD ?                     
         BNH   CDUPBMP             NO - TEST NEXT                               
         LA    R3,CCCODES          R3-> 1ST 12-BYTE TABLE SPACE                 
CDUPTBL  DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE ?                               
         BE    CDUPSTOR            YES - NOT DUPLICATE                          
         CLC   0(L'PCOLKCOD,R3),8(R2)   COMPARE TABLE TO SCREEN CODE            
         BE    CDUPERX             DUPLICATE ERROR                              
         LA    R3,L'PCOLKCOD(R3)   R3-> NEXT TABLE SPACE                        
         B     CDUPTBL             BACK TO TEST NEXT TABLE ENTRY                
CDUPSTOR DS    0H                                                               
         MVC   0(L'PCOLKCOD,R3),8(R2)   STORE THE CODE                          
         LA    R3,L'PCOLKCOD(R3)   R3-> NEXT TABLE SPACE                        
         MVI   0(R3),X'FF'         MARK POSSIBLE END OF TABLE                   
CDUPBMP  DS    0H                                                               
         BAS   R9,BUMPFLD2         BUMP TWO FIELDS TO NEXT CODE HEADER          
         B     CDUPTOP             BACK TO TEST NEXT SCREEN CODE                
*                                                                               
CDUPOKX  DS    0H                  OK RETURN                                    
         CR    RB,RB               SET CC EQUAL                                 
         B     CCDUPCKX                                                         
CDUPERX  DS    0H                  ERROR RETURN                                 
         LA    R3,DUPCCCOD         DUPLICATE CODE                               
         LTR   RB,RB               SET CC NE                                    
*                                                                               
CCDUPCKX DS    0H                                                               
         XIT1  REGS=(R2,R3)                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPBUY20 - CUSTOM COLUMN DATA EDIT ROUTINE (CCEDIT)'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCEDIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY           SAVE BUY KEY                                 
         LA    R2,CCLCOD1H         POINT R2 TO 1ST CUST COL "CODE" HDR          
         ZAP   LNCTR,=P'1'         INIT LINE COUNTER                            
*                                                                               
CCEDTOP  DS    0H                                                               
         CLI   8(R2),C'='          BROWSE ?                                     
         BNE   CCEDTOPB            NO                                           
*                                                                               
         MVI   SVBROWSE,C'B'       SET BROWSE INDICATOR                         
*                                                                               
         GOTOR VPBROWSE,DMCB,ACOMFACS,BASERD,(R2),                     +        
               0,(REC+2,C' CCR'),0                                              
*                                                                               
         DC    H'0'                BROWSE SHOULD HAVE TAKEN IT                  
*                                                                               
CCEDTOPB DS    0H                                                               
*                                                                               
         CLI   8(R2),C' '          ANYTHING IN CODE FIELD ?                     
         BH    CCEDTOPG            YES - LOOK FOR CUST COL RECORD               
         BAS   R9,BUMPFLD          NEXT SCREEN FIELD (R2-> DATA HEADER)         
         B     CCEBKUP             NEXT "LINE"                                  
*                                                                               
CCEDTOPG DS    0H                                                               
*                                                                               
         ZAP   FLDCTR,=P'1'        INDICATE FIRST FIELD ON LINE                 
*                                                                               
         CLI   8(R2),C'!'          IF NOT A SHRIEK THEN                         
         BNE   CCEDTOP0               NORMAL CUSTOM COLUMN                      
*                                                                               
*                                  ELSE STANDARD COLUMN AND ON GENFIL           
*                                                                               
*        IF !FXRATE CODE, CHECK FOR FX PROFILE                                  
*                                                                               
         CLC   8(7,R2),=C'!FXRATE'     IF NOT !FXRATE                           
         BNE   CCEDTOPK            JUST CHECK GENFIL                            
*                                                                               
         XC    SVFXPROF,SVFXPROF   FOREIGN EXCHANGE PROFILE VALUES              
         XC    WORK,WORK                                                        
         MVC   WORK+00(04),=C'P0FX'                                             
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL    NOTE: NO CLT OFFICE FOR FX PROFILE           
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK),SVFXPROF,VDATAMGR                         
         CLI   SVFXPROF+00,C'Y'                                                 
         BE    CCEDTOPK            OKAY                                         
         LA    R3,FXPRFERR                                                      
         B     CCEDERX             FX PROFILE NOT SET TO USE FX FEATURE         
*                                                                               
*                                                                               
CCEDTOPK DS    0H                                                               
*                                                                               
         XC    GKEY,GKEY                                                        
         LA    R4,GKEY                                                          
         USING GCOLKEY,R4                                                       
*                                                                               
         MVC   GCOLKRID,=AL3(GCOLKRIQ)  SET GENDIR PASSIVE KEY ID               
         MVI   GCOLKMED,C'A'       MEDIA ALWAYS A FOR CUST COL'S                
         MVI   GCOLKRCD,PCOLKRCQ   RECORD CODE FOR PASSIVE KEY                  
         MVC   GCOLKCOD,8(R2)      CC CODE                                      
         OC    GCOLKCOD,=12C' '    FORCE UPPERCASE AND BLANK FILL               
*                                                                               
         MVC   GKEYSAVE,GKEY                                                    
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'GENDIR',          X        
               GKEY,GKEY,(TERMNAL,0)                                            
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         CLC   GCOLKEY,GKEYSAVE                                                 
         BE    *+12                                                             
         LA    R3,CCRECNF          CUSTOM COLUMN REC NOT FOUND                  
         B     CCEDERX                                                          
*                                                                               
         LA    R4,CCREC            100-BYTE AREA FOR CUSTOM COL REC             
         ST    R4,AREC                                                          
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'GENFILE',         X        
               GKEY+36,AREC,(TERMNAL,DMWORK)                                    
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         LA    R4,GCOLFRST         POINT TO FIRST ELEMENT                       
*                                                                               
         B     CCEDTOP1                                                         
*                                                                               
*        CUSTOM COLUMN ON PRTFILE                                               
*                                                                               
CCEDTOP0 DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PCOLRECD,R4         GET CUSTOM COLUMN RECORD                     
*                                                                               
         MVC   PCOLKAGY,AGYALPHA AGENCY                                         
         MVI   PCOLKMED,C'A'     MEDIA ALWAYS A FOR CUST COL'S                  
         MVI   PCOLKRCD,PCOLKRCQ RECORD CODE FOR PRIMARY KEY                    
         MVC   PCOLKCOD,8(R2)    RECORD IDENTIFIER                              
         OC    PCOLKCOD,=CL12' '                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'PCOLPKEY),KEYSAVE                                          
         BE    *+12                                                             
         LA    R3,CCRECNF          CUSTOM COLUMN REC NOT FOUND                  
         B     CCEDERX                                                          
*                                                                               
         LA    R4,CCREC            100-BYTE AREA                                
         ST    R4,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R4,PCOLFRST         POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
CCEDTOP1 DS     0H                                                              
*                                                                               
         USING PCOLELEM,R4                                                      
*                                                                               
         LA    R3,CCRECNG          INVALID CUSTOM COLUMN RECORD                 
         CLC   PCOLSQN,=X'4040'    SPACES ?                                     
         BE    CCEDERX             YES - ERROR                                  
         CLC   PCOLSQN,=X'03E7'    GT 999 ? (STARTING SEQ# IS 1000)             
         BNH   CCEDERX             NO - ERROR                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  EDIT THIS CODE FOR BUY MEDIA                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LA    R3,NVMEDERR         NOT VALID FOR THIS MEDIA                     
         LA    RE,REC              POINT TO BUY RECORD                          
         USING PBUYREC,RE                                                       
* * * *  TM    PCOLMED,PCOLMXXQ    HAVE EXTENDED MEDIA INDICATOR?               
* * * *  JL    CCMEDC10                                                         
         CLI   PCOLMED,PCOLM_AQ    ALL MEDIA?                                   
         JL    CCMEDC10                                                         
         J     CCMEDC_X            RETRO "ALL" MEDIA CODE ADJUSTMENT            
CCMEDC10 CLI   PBUYKMED,C'I'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_IQ    INTERACTIVE BIT "ON" ?                       
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'M'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_MQ    MAGAZINE BIT "ON" ?                          
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'N'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_NQ    NEWSPAPER BIT "ON" ?                         
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'O'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_OQ    OUT OF HOME BIT "ON" ?                       
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'S'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_SQ    SUPPLEMENT BIT "ON" ?                        
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'T'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_TQ    TRADE BIT "ON" ?                             
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'L'                                                    
         BNE   *+12                                                             
         TM    PCOLMED,PCOLM_LQ    SOCIAL BIT "ON" ?                            
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'B'                                                    
         BNE   *+12                                                             
         TM    PCOLMED2,PCOLM_BQ   MOBILE BIT "ON" ?                            
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'V'                                                    
         BNE   *+12                                                             
         TM    PCOLMED2,PCOLM_VQ   NVIDEO BIT "ON" ?                            
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'W'                                                    
         BNE   *+12                                                             
         TM    PCOLMED2,PCOLM_WQ   LVIDEO BIT "ON" ?                            
         BNO   CCEDERX             NO - ERROR                                   
         CLI   PBUYKMED,C'D'                                                    
         BNE   *+12                                                             
         TM    PCOLMED2,PCOLM_DQ   DIGITAL AUDIO BIT "ON" ?                     
         BNO   CCEDERX             NO - ERROR                                   
*                                  CODE IS VALID FOR THIS MEDIA                 
CCMEDC_X DS    0H                                                               
*                                                                               
         DROP  RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         BAS   R9,BUMPFLD        NEXT SCREEN FIELD (R2->"DATA" HEADER)          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ZAP   FLDCTR,=P'2'        SECOND FIELD ON LINE                         
*                                                                               
         LA    R3,CCNODATA         COLUMN DATA IS MISSING                       
         CLI   8(R2),C' '          ANYTHING "THERE" ?                           
         BNH   CCEBKUP            NO - SKIP                                     
*                                                                               
         LA    R3,DEMAXERR         DATA EXCEEDS MAXIMUM LENGTH                  
*                                                                               
         BAS   RE,GETLNTH          LENGTH OF DATA RETURNED IN DTALNTH           
*                                                                               
         CLC   DTALNTH,PCOLMLEN                                                 
         BH    CCEDERX             ERROR EXIT                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   PCOLTYP,C'T'        TEXT ?                                       
         BE    CCEBKUP             YES - THIS LINE DONE                         
         CLI   PCOLTYP,C'P'        PERIOD ?                                     
         BE    CCEDTES             YES                                          
         CLI   PCOLTYP,C'D'        DATE ?                                       
         BE    CCEDTES             YES                                          
*                                  ONLY NUMERIC TYPES ARE LEFT                  
         BAS   RE,GETDEC           GET A COUNT OF DECIMALS                      
         CLC   SCOLDECS,PCOLDECS                                                
         BNH   *+12                                                             
         LA    R3,DEC#PERR         INVALID NO OF DECIMAL PLACES                 
         B     CCEDERX                                                          
*                                                                               
         ZIC   R3,DTALNTH                                                       
*                                                                               
         MVI   SCOLDECS,C'0'       SET # OF DECIMALS TO 0                       
         CLI   PCOLDECS,0          IS ZERO THE ACTUAL NUMBER ?                  
         BE    CCEDPCT             YES                                          
         MVC   SCOLDECS,PCOLDECS   NON-ZERO NUMBER OF DECIMAL PLACES            
         OI    SCOLDECS,X'80'      FOR DOUBLEWORD PACKED CASHVAL OUTPUT         
*                                                                               
CCEDPCT  DS    0H                  MODIFY CASHVAL EDIT FOR % SIGN               
         CLI   PCOLTYP,C'%'        PERCENT ?                                    
         BNE   CCEDVC              NO - MUST BE NUMERIC(N) OR CASH($)           
         LA    RE,8(R2)                                                         
         AR    RE,R3                                                            
         AHI   RE,-1               RE -> LAST CHARACTER IN DATA FIELD           
         CLI   0(RE),C'%'          PERCENT SIGN ?                               
         BNE   CCEDVC              NO                                           
         AHI   R3,-1               REDUCE DATA LENGTH FOR CASHVAL               
*                                                                               
CCEDVC   DS    0H                  VALIDATE NUMERIC INPUT                       
         GOTOR VCASHVAL,DMCB,(SCOLDECS,8(R2)),(R3)                              
*                                                                               
         CLI   0(R1),X'00'                                                      
         BNE   CCENGNUM            NO ERRORS TOLERATED                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*     DISPLAY NUMERIC FIELD AND RE-EDIT (COMMAS NOW INCLUDED)                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         MVC   WORK2(8),4(R1)      O/P FROM CASHVAL TO WORK2                    
*                                                                               
         XC    WORK,WORK           PREP FOR EDITOR USE                          
         LA    R3,WORK                                                          
         USING EBLOCKD,R3                                                       
         LA    RF,WORK2            SET UP EDITOR INPUT BLOCK                    
         ST    RF,EBAIN            INPUT ADDRESS                                
         MVI   EBLIN,8             INPUT LENGTH (ALWAYS PL8)                    
         MVI   EBTIN,C'P'          INPUT TYPE (PACKED)                          
         LA    RF,8(R2)            SET UP EDITOR OUTPUT BLOCK                   
         ST    RF,EBAOUT           OUTPUT ADDRESS                               
         MVC   EBDECS,PCOLDECS     NO OF DECIMAL PLACES                         
         MVI   EBALIGN,C'L'        LEFT ALIGN                                   
*SMY*    MVC   EBLOUT,PCOLMLEN     OUTPUT LENGTH                                
         MVI   EBLOUT,20           MAXIMUM OUTPUT LENGTH                        
         OI    EBOPT,EBOQCEY       COMMAS=YES                                   
         MVI   EBFLOAT,C'-'        MINUS =YES                                   
*                                                                               
         GOTOR VEDITOR,DMCB,EBLOCK                                              
*                                                                               
         DROP  R3                                                               
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
*              NOW SEE IF FIELD IS TOO LONG WITH COMMAS, ETC., INCLUDED         
*                                                                               
         LA    R3,DEMAXERR         DATA EXCEEDS MAXIMUM LENGTH                  
         BAS   RE,GETLNTH          LENGTH OF DATA RETURNED IN DTALNTH           
*                                                                               
         CLC   DTALNTH,PCOLMLEN                                                 
         BH    CCEDERX             ERROR EXIT                                   
         B     CCEBKUP             NO ERRORS - GO TEST NEXT "LINE"              
*                                                                               
*                                  OUTPUT ERROR MESSAGES                        
CCENGNUM DS    0H                  INVALID NUMERIC FIELD                        
         LA    R3,CCNGPCT          INVALID PERCENT                              
         CLI   PCOLTYP,C'%'        PERCENT ?                                    
         BE    CCEDERX             YES                                          
*                                                                               
         LA    R3,DATNT#ER         INVALID (N)UMERIC                            
         CLI   PCOLTYP,C'N'        NUMERIC ?                                    
         BE    CCEDERX             YES                                          
*                                                                               
         LA    R3,CCNGCSH          INVALID CASH AMOUNT                          
         CLI   PCOLTYP,C'$'        CASH ?                                       
         BE    CCEDERX             YES                                          
*                                                                               
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
CCEDTES  DS    0H                  EDIT PERIOD OR DATE                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING PERVALD,R5          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
         CLI   PCOLTYP,C'P'        PERIOD ?                                     
         BE    CCEPERD             YES                                          
*                                  SINGLE (D)ATE ONLY                           
         GOTOR VPERVAL,DMCB,(DTALNTH,8(R2)),(X'C0',(R5))                        
         CLI   4(R1),X'04'         ONLY ONE DATE INPUT ?                        
         BNE   CCEDTNG             INVALID DATE OR PERIOD                       
*                                                                               
         MVC   BSTART,PVALBSTA     SAVE BINARY FORMAT                           
         XC    BEND,BEND                                                        
         B     CCEBKUP             NEXT LINE                                    
*                                                                               
CCEPERD  DS    0H                  PERIOD VALIDATION                            
*                                                                               
         GOTOR VPERVAL,DMCB,(DTALNTH,8(R2)),(X'80',(R5))                        
         CLI   4(R1),X'00'         NO ERRORS TOLERATED                          
         BNE   CCEDTNG             INVALID DATE OR PERIOD                       
*                                                                               
         MVC   BSTART,PVALBSTA     SAVE BINARY FORMAT                           
         MVC   BEND,PVALBEND                                                    
*                                                                               
         CLC   BEND,BSTART         START BEFORE END ?                           
         BNL   CCEBKUP             NO - NEXT LINE                               
*                                                                               
         DROP  R5                                                               
*                                                                               
CCEDTNG  DS    0H                  DATE OR PERIOD INVALID                       
         LA    R3,CCNGPRD          INVALID PERIOD (DATE RANGE)                  
         CLI   PCOLTYP,C'P'        PERIOD ?                                     
         BE    *+8                 YES                                          
         LA    R3,CCNGDTE          INVALID DATE                                 
         B     CCEDERX                                                          
*                                                                               
CCEBKUP  DS    0H                  BACK TO EDIT NEXT LINE                       
         LA    RF,CCLTEXTH         RF-> END OF SCREEN INPUT LINES               
         BAS   R9,BUMPFLD          NEXT SCREEN FIELD (R2-> CODE HEADER)         
         CR    R2,RF               END OF SCREEN INPUT ?                        
         BNL   CCEDOKX             END OF SCREEN                                
*                                                                               
         AP    LNCTR,=P'1'         BUMP TO NEXT LINE ON SCREEN                  
*                                                                               
         B     CCEDTOP             NO - BACK TO TOP FOR ANOTHER LINE            
*                                  YES - DONE                                   
CCEDOKX  DS    0H                  OK RETURN                                    
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
         CR    RB,RB               SET CC EQUAL                                 
         B     CCEDXX                                                           
CCEDERX  DS    0H                  ERROR RETURN                                 
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
         LTR   RB,RB               SET CC NE                                    
*                                                                               
CCEDXX   DS    0H                                                               
         XIT1  REGS=(R2,R3)                                                     
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
      TITLE 'PPBUY20 - CUSTOM COLUMN DATA CHANGE ROUTINE (CCCHANGE)'            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        GO THROUGH PREVIOUSLY VALIDATED SCREEN AND UPDATE                      
*           THE CUSTOM COLUMN ELEMENTS IN REC                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCCHANGE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,CCLTRH                                                        
         LA    R3,DATALOCK                                                      
         B     CCHAERXX            ERROR EXIT                                   
*                                                                               
*        NOW WE DELETE ONLY IF DATA PORTION IS EMPTY                            
*                                                                               
******   BRAS  RE,CCDELEL          DELETE CUST COL ELEMS DISPLAYED              
*                                    AND SAVED IN SVSCRCC                       
         BAS   RE,CLRREC           CLEAR END OF RECORD                          
*                                                                               
         MVC   SVKEY,KEY           SAVE BUY KEY                                 
         LA    R2,CCLCOD1H         POINT TO FIRST CODE FIELD                    
*                                                                               
CCCHA10  DS    0H                                                               
         CLI   8(R2),C' '          ANYTHING IN CODE FIELD ?                     
         BH    CCCHA20             YES - GO PROCESS                             
         BAS   R9,BUMPFLD2         BUMP PAST DATA TO NEXT CODE FIELD            
         LA    RF,CCLTEXTH         RF-> END OF SCREEN INPUT LINES               
         CR    R2,RF                                                            
         BNL   CCHAOKX             DONE WITH SCREEN - RETURN                    
         B     CCCHA10             TEST NEXT CODE FIELD                         
*                                                                               
*                                  R2-> CODE FIELD HEADER                       
CCCHA20  DS    0H                  GET CUSTOM COLUMN RECORD                     
*                                                                               
         ST    R2,FULL             SAVE ADDRESS OF CODE FIELD                   
*                                                                               
         CLI   8(R2),C'!'          IF NOT A SHRIEK THEN                         
         BNE   CCCHA21                NORMAL CUSTOM COLUMN                      
*                                                                               
*                                  ELSE STANDARD COLUMN AND ON GENFIL           
*                                                                               
         XC    GKEY,GKEY                                                        
         LA    R4,GKEY                                                          
         USING GCOLKEY,R4                                                       
*                                                                               
         MVC   GCOLKRID,=AL3(GCOLKRIQ)  SET GENDIR PASSIVE KEY ID               
         MVI   GCOLKMED,C'A'       MEDIA ALWAYS A FOR CUST COL'S                
         MVI   GCOLKRCD,PCOLKRCQ   RECORD CODE FOR PASSIVE KEY                  
         MVC   GCOLKCOD,8(R2)      CC CODE                                      
         OC    GCOLKCOD,=12C' '    FORCE UPPERCASE AND BLANK FILL               
*                                                                               
         MVC   GKEYSAVE,GKEY                                                    
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'GENDIR',          X        
               GKEY,GKEY,(TERMNAL,0)                                            
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         CLC   GCOLKEY,GKEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    R4,CCREC            100-BYTE AREA FOR CUSTOM COL REC             
         ST    R4,AREC                                                          
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'GENFILE',         X        
               GKEY+36,AREC,(TERMNAL,DMWORK)                                    
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+10                                                             
         SR    R3,R3               CLEAR ERROR NUMBER                           
         J     ERROR                                                            
*                                                                               
         LA    R4,GCOLFRST         POINT TO FIRST ELEMENT                       
*                                                                               
         B     CCCHA22                                                          
*                                                                               
CCCHA21  DS    0H                                                               
*                                                                               
*        CUSTOM COLUMN ON PRTFILE                                               
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PCOLRECD,R4                                                      
*                                                                               
         MVC   PCOLKAGY,AGYALPHA AGENCY                                         
         MVI   PCOLKMED,C'A'     MEDIA ALWAYS A FOR CUST COL'S                  
         MVI   PCOLKRCD,PCOLKRCQ RECORD CODE FOR PRIMARY KEY                    
         MVC   PCOLKCOD,8(R2)    RECORD IDENTIFIER                              
         OC    PCOLKCOD,=CL12' '                                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'PCOLPKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                  GET THE CUSTOM COLUMN RECORD                 
         LA    R4,CCREC            100-BYTE AREA                                
         ST    R4,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R4,PCOLFRST         POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
CCCHA22  DS    0H                                                               
*                                                                               
         USING PCOLELEM,R4         ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         MVC   SCOLTYP,PCOLTYP     SAVE CUSTOM COLUMN TYPE                      
         MVC   SCOLSQN,PCOLSQN       AND UNIQUE SEQUENCE NUMBER                 
         MVC   SCOLDECS,PCOLDECS     AND NUMBER OF DECIMALS                     
         MVC   SCOLTRK,PCOLTRK       AND IDESK TRACKING OPTION                  
         DROP  R4                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    CREATE AND ADD CUSTOM COLUMN ELEMENT TO BUY REC            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         BAS   R9,BUMPFLD          R2-> SCREEN DATA FIELD HEADER                
*                                                                               
         XC    WKELEM,WKELEM                                                    
         LA    R4,WKELEM                                                        
         USING BYCCELD,R4          BUY CUSTOM COLUMN ELEMENT                    
         MVI   BYCCELM,BYCCIDQ     ELEMENT ID                                   
         MVC   BYCCSQN,SCOLSQN     UNIQUE SEQUENCE NUMBER                       
*                                                                               
         CLI   SCOLTRK,C'Y'        IF TRACKING OPTION ON                        
         BNE   *+8                                                              
         OI    BYCCSWS,BYCCTRKQ       SET SWITCH IN ELEMENT                     
*                                                                               
         CLI   5(R2),0             SKIP IF NO DATA ENTERED                      
         BE    CCCHA40X                                                         
         CLI   5(R2),1             OR I BYTE OF SPACE                           
         BNE   *+12                                                             
         CLI   8(R2),C' '                                                       
         BNH   CCCHA40X                                                         
*                                                                               
         CLI   SCOLTYP,C'T'        TEXT ?                                       
         BE    CCCHA30             YES - FINISH TEXT ELEMENT                    
         CLI   SCOLTYP,C'P'        PERIOD ?                                     
         BE    CCCHA40             YES - FINISH "DATES" ELEMENT                 
         CLI   SCOLTYP,C'D'        DATE ?                                       
         BE    CCCHA40             YES - FINISH "DATES" ELEMENT                 
*                                                                               
*                                  ONLY NUMERIC TYPES ARE LEFT                  
*                                                                               
         BAS   RE,GETLNTH          GET SCREEN FIELD LENGTH (COL DATA)           
         ZIC   RF,DTALNTH          COLUMN DATA LENGTH                           
*                                                                               
         CLI   SCOLDECS,0          IS ZERO THE ACTUAL # OF DECIMALS             
         BNE   *+12                NO                                           
         MVI   SCOLDECS,C'0'       SET DECIMAL # TO C'0' FOR CASHVAL            
         B     *+8                                                              
         OI    SCOLDECS,X'80'      FOR DOUBLEWORD PACKED CASHVAL OUTPUT         
*                                  MODIFY CASHVAL EDIT FOR % SIGN               
         CLI   SCOLTYP,C'%'        PERCENT ?                                    
         BNE   CCCHA26             NO - MUST BE NUMERIC(N) OR CASH($)           
         LA    RE,8(R2)                                                         
         AR    RE,RF                                                            
         AHI   RE,-1               RE -> LAST CHARACTER IN DATA FIELD           
         CLI   0(RE),C'%'          PERCENT SIGN ?                               
         BNE   CCCHA26             NO                                           
         AHI   RF,-1               REDUCE DATA LENGTH FOR CASHVAL               
*                                                                               
CCCHA26  DS    0H                                                               
         GOTOR VCASHVAL,DMCB,(SCOLDECS,8(R2)),(RF)                              
         CLI   0(R1),X'00'                                                      
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    RF,BYCCHDRL         ELEMENT "HEADER LENGTH"                      
         AHI   RF,8                ALWAYS PL8 FOR NUMERIC ELEMENTS              
         STC   RF,BYCCLEN                                                       
         MVC   BYCCDATA(8),4(R1)   O/P FROM CASHVAL TO ELEMENT                  
         B     CCCHA40X            ADD ELEMENT TO BUYREC                        
*                                                                               
CCCHA30  DS    0H                  FINISH THE TEXT ELEMENT                      
         ZIC   RE,5(R2)            COLUMN DATA LENGTH                           
         LA    RF,BYCCHDRL         ELEMENT "HEADER LENGTH"                      
         AR    RF,RE               RF=ELEMENT LENGTH                            
         STC   RF,BYCCLEN                                                       
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BYCCDATA(0),8(R2)   SCREEN TEXT TO ELEMENT                       
         B     CCCHA40X            ADD ELEMENT TO BUYREC                        
*                                                                               
CCCHA40  DS    0H                  FINISH THE "DATES" ELEMENT                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING PERVALD,R5          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
         CLI   SCOLTYP,C'P'        PERIOD ?                                     
         BE    CCCHA40P            YES                                          
*                                  NO - SINGLE (D)ATE ONLY                      
         GOTOR VPERVAL,DMCB,(5(R2),8(R2)),(X'E0',(R5))                          
         CLI   4(R1),X'04'         ONLY ONE DATE INPUT ?                        
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    RF,BYCCHDRL         ELEMENT "HEADER LENGTH"                      
         AHI   RF,3                FOR BINARY YMB                               
         STC   RF,BYCCLEN                                                       
         MVC   BYCCDATA(3),PVALBSTA      O/P FROM PERVAL TO ELEMENT             
         B     CCCHA40X            ADD ELEMENT TO BUYREC                        
*                                                                               
CCCHA40P DS    0H                  PERIOD                                       
         GOTOR VPERVAL,DMCB,(5(R2),8(R2)),(X'80',(R5))                          
         CLI   4(R1),X'00'         NO ERRORS TOLERATED                          
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    RF,BYCCHDRL         ELEMENT "HEADER LENGTH"                      
         AHI   RF,6                FOR BINARY YMBYMB (START-END)                
         STC   RF,BYCCLEN                                                       
         MVC   BYCCDATA(6),PVALBSTA      O/P FROM PERVAL TO ELEMENT             
*                                                                               
         DROP  R5                                                               
CCCHA40X DS    0H                  SEE IF ELEMENT TO BE ADDED FROM              
         LA    R5,REC+33             WKELEM IS A DUPLICATE (ERROR)              
         CLI   0(R5),X'20'         BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
         MVI   ELCODE,X'CC'                                                     
CCCHA50  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    *+12                                                             
         OI    CHGIND5,PCHGTRKQ    INDICATE CHANGE IN SOME CC                   
         B     CCCHA50X            DONE - NOT A DUPLICATE                       
*                                                                               
         CLC   BYCCSQN,2(R5)       MATCHING UNIQUE SEQUENCE # ?                 
         BNE   CCCHA50             NO - TEST NEXT                               
*                                                                               
         XC    SVELEM,SVELEM                                                    
*                                                                               
         CLC   =X'200F',2(R5)      FXRATE STANDARD CUSTOM COLUMN CODE?          
         BNE   CCCHA50M                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVELEM(0),0(R5)     SAVE OLD FX RATE                             
*                                                                               
CCCHA50M DS    0H                                                               
*                                                                               
*        CHECK FOR CHANGE IN CCOL                                               
*                                                                               
         CLC   BYCCLEN,1(R5)       TEST FOR NEW ELEMENT LENGTH                  
         BE    *+12                                                             
         OI    CHGIND5,PCHGTRKQ    NO - INDICATE A CHANGE                       
         B     CCCHA50N                                                         
*                                                                               
         LLC   RF,BYCCLEN          GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FO EXECUTE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BYCCELM(0),0(R5)    TEST IF OLD AND NEW THE SAME                 
         BE    *+8                                                              
         OI    CHGIND5,PCHGTRKQ    NO - INDICATE A CHANGE                       
*                                                                               
CCCHA50N DS    0H                                                               
*                                                                               
*        DELETE OLD ELEMENT                                                     
*                                                                               
         GOTOR VRECUP,DMCB,(1,REC),0(R5)                                        
*                                                                               
****     LA    R3,DUPCCCOD         DUPLICATE CODE ERROR                         
****     L     R2,FULL             FULL=(A)CODE FIELD HEADER                    
****     B     CCHAERX             ERROR EXIT                                   
         DROP  R4                                                               
*                                                                               
CCCHA50X DS    0H                  WKELEM HAS NEW ELEMENT                       
         CLI   5(R2),0             DON'T ADD IF NO DATA                         
         BE    CCCHA60                                                          
         CLI   5(R2),1             OR I BYTE OF SPACE                           
         BNE   *+12                                                             
         CLI   8(R2),C' '                                                       
         BNH   CCCHA60                                                          
*                                                                               
*                                                                               
*        ADD ELEMENT WHERE OLD ONE WAS                                          
*                                                                               
*****    BAS   RE,PEOREC           POINT R5 TO END OF BUY RECORD                
         GOTOR VRECUP,DMCB,(1,REC),WKELEM,(R5)                                  
*                                                                               
         CLC   =X'200F',WKELEM+2   FXRATE STANDARD CUSTOM COLUMN CODE?          
         BNE   CCCHA60                                                          
         SR    RE,RE                                                            
         IC    RE,WKELEM+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVELEM(0),WKELEM    SAVE OLD FX RATE                             
         BE    CCCHA60                                                          
         OI    CHGIND5,PCHFXRTQ                                                 
*                                                                               
CCCHA60  DS    0H                                                               
*                                                                               
         BAS   R9,BUMPFLD        NEXT SCREEN FIELD (R2->"CODE" HEADER)          
*                                                                               
         B     CCCHA10             BACK TO TOP                                  
*                                                                               
CCHAOKX  DS    0H                  OK RETURN                                    
*                                                                               
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
         CR    RB,RB               SET CC EQUAL                                 
         B     CCHAXIT                                                          
CCHAERX  DS    0H                  ERROR RETURN                                 
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
CCHAERXX LTR   RB,RB               SET CC NE                                    
*                                                                               
CCHAXIT  DS    0H                                                               
         XIT1  REGS=(R2,R3)                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCUPDTE  NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
         MVC   AREC,AWRKREC        POINT TO WRKREC                              
         BAS   RE,GETREC           READ FOR UPDATE INTO WRKREC BUT . .          
*                                                                               
         LA    RE,REC              UPDATE FROM ALREADY MODIFIED REC             
         ST    RE,AREC                                                          
*                                                                               
         CLI   REC+33,X'20'        BUY RECORD ?                                 
         BE    *+6                                                              
         DC    H'0'                WRONG RECORD TYPE                            
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,CCLTRH                                                        
         LA    R3,DATALOCK                                                      
         B     CCUPERXX            ERROR EXIT                                   
*                                                                               
         BAS   RE,PUTREC           ELEM(S) ADDED AND BUY REC UPDATED            
*                                                                               
******************************************************************              
*   UPDATE THE CUSTOM COLUMN RECORD(S) - FLAG AS "USED IN BUY"                  
******************************************************************              
*                                                                               
         MVC   SVKEY,KEY           SAVE BUY KEY                                 
         LA    R2,CCLCOD1H         POINT R2 TO 1ST CUST COL "CODE" HDR          
*                                                                               
CCUPTOP  DS    0H                                                               
         LA    RF,CCLTEXTH         RF-> END OF SCREEN INPUT LINES               
         CLI   8(R2),C' '          ANYTHING IN CODE FIELD ?                     
         BH    CCUPTOPG            YES - GET THE CUST COL RECORD                
         BAS   R9,BUMPFLD2         BUMP TWO FIELDS TO NEXT CODE HEADER          
         CR    R2,RF               END OF SCREEN INPUT ?                        
         BL    CCUPTOP             NO - BACK TO TOP TO TEST NEXT                
         B     CCUPOKX             YES - DONE                                   
*                                                                               
CCUPTOPG DS    0H                                                               
*                                                                               
         CLI   8(R2),C'!'          IF A SHRIEK THEN                             
         BE    CCUPNXT                STANDARD COLUMN AND NOT FLAGGED           
*                                     AS USED IN BUY                            
*        CUTOM COLUMN ON PRTFILE                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCOLRECD,R4         GET CUSTOM COLUMN RECORD                     
*                                                                               
         MVC   PCOLKAGY,AGYALPHA AGENCY                                         
         MVI   PCOLKMED,C'A'     MEDIA ALWAYS A FOR CUST COL'S                  
         MVI   PCOLKRCD,PCOLKRCQ RECORD CODE FOR PRIMARY KEY                    
         MVC   PCOLKCOD,8(R2)    RECORD IDENTIFIER                              
         OC    PCOLKCOD,=CL12' '                                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'PCOLPKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
         LA    R4,CCREC            I/O AREA                                     
         ST    R4,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R4,PCOLFRST         POINT TO FIRST ELEMENT                       
         USING PCOLELEM,R4         ESTABLISH AS DESCRIPTION ELEMENT             
*                                                                               
         TM    PCOLSTAT,X'80'      ALREADY FLAGGED FOR "USED IN BUY" ?          
         BO    CCUPNXT             YES - GO TEST NEXT CODE                      
*                                  NO                                           
         OI    PCOLSTAT,X'80'      FLAG THE RECORD NOW AND                      
         BAS   RE,PUTREC           WRITE THE RECORD                             
*                                                                               
         DROP  R4                                                               
*                                                                               
CCUPNXT  DS    0H                                                               
         BAS   R9,BUMPFLD2         BUMP TWO FIELDS TO NEXT CODE HEADER          
         B     CCUPTOP             BACK TO TOP FOR ANOTHER LINE                 
*                                                                               
CCUPOKX  DS    0H                  OK RETURN                                    
         MVC   KEY,SVKEY                                                        
         BAS   RE,HIGH             RESTORE KEY                                  
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
CCUPXX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
CCUPERXX LTR   RB,RB               SET CC NE                                    
         XIT1  REGS=(R2,R3)                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPCLR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CCLCOD1H         POINT TO 1ST FLD ON CUSTOM COL LIST          
         LA    RF,10               TEN LINES TO BE CLEARED                      
*                                                                               
DSPCLR30 XC    8(12,R2),8(R2)      CLR CODE                                     
         NI    1(R2),X'FF'-X'20'   INIT PROTECTION                              
         OI    6(R2),X'80'                                                      
         BAS   R9,BUMPFLD                                                       
         XC    8(60,R2),8(R2)      CLR DATA                                     
         OI    6(R2),X'80'                                                      
         BAS   R9,BUMPFLD                                                       
         BCT   RF,DSPCLR30                                                      
DSPCLRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*    DELETE ANY CUSTOM COLUMN ELEMENTS BY THEIR UNIQUE SEQUENCE #     *         
*    THAT HAVE DEEN DISPLAYED IN THE IMMEDIATELY PREVIOUS RECALL      *         
*    THESE NUMBERS ARE STORED (UP TO 10) ON EACH RECALL IN A TWA      *         
*    TABLE LABELED SVSCRCC                                            *         
*    THESE WILL BE REPLACED BY THE ELEMENTS (IF ANY) APPEARING        *         
*    ON THE "CURRENT" SCREEN                                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCDELEL  NTR1  BASE=*,LABEL=*      DEL CUSTOM COL'S ELEM FROM BUY REC           
         LA    R3,10               LOOP COUNTER                                 
         LA    R4,SVSCRCC          POINT TO 1ST BYCCSQN SAVED                   
CCDEL20  DS    0H                                                               
         OC    0(2,R4),0(R4)       ANYTHING THERE ?                             
         BZ    CCDELX              NO - DONE                                    
*                                                                               
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'         BUY DESCRIPTION ELEM?                        
         BE    *+6                                                              
         DC    H'0'                WORKING WITH WRONG RECORD!                   
         MVI   ELCODE,X'CC'                                                     
CCDEL40  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         CLC   0(2,R4),2(R5)       MATCHING UNIQUE SEQUENCE # ?                 
         BNE   CCDEL40             NO - TEST NEXT                               
*                                  YES - DELETE IT                              
         GOTOR VRECUP,DMCB,(1,REC),0(R5)                                        
         LA    R4,2(R4)            BUMP TO NEXT SAVED BYCCSQN #                 
         BCT   R3,CCDEL20          SEE IF MORE TO DELETE                        
CCDELX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTOR (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTOR (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTOR (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLKEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTOR (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
TSTLKEQ  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               NOT EQUAL                                    
         J     DSPCLRX                                                          
*                                                                               
         DROP  L                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLKWKA DS    0H                                                               
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WKT41120 DSECT                     WORKING STORAGE AREA DSECT                   
*                                                                               
WKAREA   DS    0H                  BEGINNING OF WORKING STORAGE AREA            
*                                                                               
CCREC    DS    CL100               X'61' CUSTOM COL REC I/O AREA                
WKELEM   DS    CL255               FOR BUILDING ELEMS OR OTHER USES             
WORK2    DS    CL60                ANOTHER WORK FIELD                           
SVKEY    DS    CL48                                                             
DATADISP DS    H                                                                
WKSAVER  DS    F                   FOR TEMPORARY REGISTER SAVES                 
WKSVDSCH DS    F                   SAVE REGISTER FOR DISP/CHANGE LOGIC          
FFLDADDR DS    F                   TO SAVE ADDRESS OF FIRST FLD ON SCR          
ERRSW    DS    C                   GENERAL SWITCH USED TO CONTROL ERRS          
DMGSW    DS    C                   DATA MGR RESTORING REC SWITCH                
TSTMED   DS    X                   BUY MEDIA IN BIT FORMAT                      
*                                  (X'40'=M, X'20'=N, ETC.)                     
DTALNTH  DS    X                   LENGTH OF CODE DATA                          
SCOLTYP  DS    C                   CUSTOM COLUMN TYPE                           
SCOLSQN  DS    XL2                 CUSTOM COLUMN UNIQUE SEQUENCE NUMBER         
SCOLDECS DS    X                   CUSTOM COLUMN DECIMAL PLACES                 
SCOLTRK  DS    X                   CUSTOM COLUMN TO BE TRACKED                  
BSTART   DS    XL3                 START DATE IN BINARY YMD                     
BEND     DS    XL3                 END   DATE IN BINARY YMD                     
*                                                                               
CHGSW    DS    C                                                                
NEWCUSCR EQU   X'80'               FIRST TR CODE IS CU                          
DATECHGD EQU   X'40'               DATE FLD HAS BEEN CHANGED                    
*                                                                               
CCACT    DS    C                   CUSTOM COL'S ACTION (CHANGE/RECALL)          
*                                                                               
ACELTAB  DS    10CL32              TEN LINES OF 32 BYTES ELEM                   
ACELTABX DS    X                   END OF TABLE MARKER                          
ACELTABQ EQU   32                  LEN OF ONE ENTRY IN ADDTNL CHRGS TAB         
*                                                                               
CCCODES  DS    10CL12              TABLE OF CUST COL CODES (10 MAX)             
CCCODESX DS    X                   END OF TABLE MARKER                          
*                                                                               
GKEY     DS    XL48                GENDIR KEYS                                  
GKEYSAVE DS    XL48                                                             
*                                                                               
LNCTR    DS    PL2                 LINE COUNTER FOR PBU ERRORS                  
FLDCTR   DS    PL2                 FIELD COUNTER FOR PBU ERRORS                 
*                                                                               
SVFXPROF DS    XL16                                                             
SVIDKPRF DS    XL16                IDESK CONTROL PROFILE VALUES                 
*                                                                               
IDESKCSW DS    X                   IDESK COLUMN SWITCH                          
IDKRECNQ EQU   X'80'               IDESK RECONCILIATION COLUMN                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SVSCREEN DS    0H                                                               
         DS    10CL(SVSCRLQ)       72 INPUT CHARS PER LINES (10 LINES)          
SVSCREEQ EQU   *-SVSCREEN                                                       
SVSCRLQ  EQU   72                  LENGTH OF ONE ENTRY                          
*                                                                               
SVACTB   DS    10CL32              TEN LINES OF 32 BYTES ELEM (SAVE)            
SVACTBX  DS    X                   END OF TABLE MARKER                          
SVACTBQ  EQU   32                  LEN OF ONE ENTRY IN ADDTNL CHRGS TAB         
*                                                                               
WKINPUT  DS    CL11                STORAGE FOR INPUT FROM SCREEN                
WKNUMSW  DS    CL1                 SWITCH                                       
SVSCRDAT DS    CL8                 SAVE DATE ON SCREEN                          
*                                                                               
SVELEM   DS    XL255                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WKAREAX  EQU   *-WKAREA            LENGTH OF WORKING AREA                       
*                                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
EBLOCKD  DSECT                     EDIT BLOCK FOR EDITOR ROUTINE                
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
         PRINT ON                                                               
PCOLKRCQ EQU   X'61'               CUSTOM COLUMN RECORD TYPE                    
PCOLELEQ EQU   X'61'               CUSTOM COLUMN ELEMENT CODE                   
PCOLPRCQ EQU   X'D1'               CUSTOM COL PASSIVE KEY RECORD TYPE           
*                                                                               
PPEFD    DSECT                     CUSTOM COLUMNS LOWER BUY SCREEN              
       ++INCLUDE PPBUYEFD                                                       
*                                                                               
FXPRFERR EQU   333                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088PPBUY20   10/17/18'                                      
         END                                                                    
