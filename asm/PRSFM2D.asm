*          DATA SET PRSFM2D    AT LEVEL 039 AS OF 11/10/08                      
*PHASE T41C2DA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C2D - ESTIMAT COPY                                           
*                                                                               
* SMYE 10/22/08 SEND ERROR MESSAGE ON DUPLICATE ENTRY                           
*                                                                               
* KWAN 06/06/05 BROWSE FUNCTION                                                 
*                                                                               
* KWAN 02/04/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 04/15/03 CONVERT ESTIMATE COPY FROM FIL TO SFM                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS COPY                                         *         
*                                                                     *         
*  INPUTS       SCREEN T41CE0 (ESTIMATE COPY)                         *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C2D - ESTIMATE COPY'                                         
*                                                                               
T41C2D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C2D,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
                                                                                
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   PFAID,0             PFKEY IS PRESSED?                            
         BE    *+12                NO                                           
         BRAS  RE,CKPFKEYS                                                      
         JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC                                                      
         BE    VR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    PPTRS                                                            
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,ECPMEDH          POINT TO MEDIA FLD                           
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   ECPMEDN,MEDNM                                                    
         OI    ECPMEDNH+6,X'80'    DISPLAY MEDIA NAME                           
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PESTKEY,R6                                                       
*                                                                               
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'      RECORD CODE FOR EST                          
*                                                                               
         LA    R2,ECPCLTH          POINT TO CLT FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         JE    INVFDERR            ALL IS NOT ALLOWED AS CLIENT CODE            
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   PESTKCLT,QCLT                                                    
         MVC   ECPCLTN,CLTNM                                                    
         OI    ECPCLTNH+6,X'80'    DISPLAY CLT NAME                             
*                                                                               
         LA    R2,ECPPRDH          POINT TO PRD FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK34                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' PRD'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK34     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL PRD?                                     
         JE    INVFDERR            ALL IS NOT ALLOWED AS PRD CODE               
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   PESTKPRD,QPRD                                                    
         MVC   ECPPRDN,PRDNM                                                    
         OI    ECPPRDNH+6,X'80'    DISPLAY PRD NAME                             
*                                                                               
         LA    R2,ECPESTH          POINT TO EST FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK54                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' EST'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK54     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         JZ    NTNUMERR                                                         
*                                                                               
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   PESTKEST,BEST       BINARY EST                                   
         MVC   ECPESTN,ESTNM                                                    
         OI    ECPESTNH+6,X'80'    DISPLAY PRD NAME                             
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA                                
*                                  FIRST CHECK FOR DUPLICATE ENTRIES            
         LA    R2,ECPPRD1H         1ST PRD INPUT FLD                            
         LA    R3,ECPPRDAH         LAST PRD INPUT FLD                           
VR00C    DS    0H                                                               
         CR    R2,R3               END OF PRD INPUT FLDS?                       
         BNL   VR05                YES - NO DUP'S                               
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR00N               NO - TEST NEXT FIELD                         
         LR    R4,R2                                                            
DUPLUP   DS    0H                                                               
         ZIC   R0,0(R4)                                                         
         AR    R4,R0               POINT TO "NEXT " ENTRY FIELD                 
         CR    R4,R3               AT END ?                                     
         BH    VR00N               YES - BUMP TO NEXT FIELD TO TEST             
         CLI   5(R4),0             ANY INPUT ?                                  
         BE    DUPLUP              NO - TEST NEXT                               
         OC    8(3,R2),SPACES                                                   
         OC    8(3,R4),SPACES                                                   
         CLC   8(3,R2),8(R4)       DUPLICATE ?                                  
         BNE   DUPLUP              NO - SEE IF MORE TO TEST AGAINST             
*                                                                               
         LR    R2,R4               POINT TO DUPLICATE ENTRY                     
         LA    R5,CONHEADH                                                      
         MVC   8(L'DUPMSG,R5),DUPMSG  "DUP - CLEAR IT AND PRESS ENTER"          
         J     TRAPERR2                                                         
*                                                                               
VR00N    BRAS  RE,DR_BFLD          BUMP TO NEXT FLD                             
         B     VR00C                                                            
*                                                                               
VR05     DS    0H                  CONTINUE VALIDATION                          
         LA    R2,ECPPRD1H         1ST PRD INPUT FLD                            
         LA    R3,ECPPRDAH         LAST PRD INPUT FLD                           
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVC   SVWORK(L'KEY),KEY                                                
*                                                                               
VR10     CR    R2,R3               END OF PRD INPUT FLDS?                       
         BH    VR25                                                             
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR20                                                             
         MVC   WKPRDCOD,SPACES     SPACE PADDED                                 
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKPRDCOD(0),8(R2)                                                
         CLC   WKPRDCOD,=C'ZZZ'                                                 
         JE    CCZZZERR            CANNOT COPY ZZZ PRODUCT                      
*                                                                               
         BRAS  RE,VR_GETPR         VALIDATE PRD CODE                            
         MVC   KEY,SVWORK                                                       
         MVC   KEY+07(03),WKPRDCOD PRD CODE FROM SCR                            
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   *+16                                                             
         TM    KEY+25,X'80'        EST REC IS DELETED?                          
         JZ    DUPLRERR            NOT DELETED, CANNOT ADD DUPLICATES           
         J     RECDLERR                                                         
VR20     BRAS  RE,DR_BFLD          BUMP TO NEXT FLD                             
         B     VR10                                                             
*                                                                               
* PRD CODES ARE VALIDATED, OK TO ADD ESTIMATES                                  
*                                                                               
VR25     NI    DMINBTS,X'FF'-X'08' RESTORE TO ORIGINAL STATE                    
         MVC   KEY,SVWORK          RESTORE KEY                                  
         L     R6,AIO              EST REC TO BE COPIED                         
         MVI   ELCODE,X'07'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                1ST EST ELEM MUST BE THERE!                  
         USING PESTELEM,R6                                                      
         MVC   WKESTTST,PESTTEST   SAVE ORIGINAL EST TEST STATUS                
         LA    R2,ECPPRD1H         POINT TO 1ST DISPLAYING FLD                  
         LA    R3,ECPPRDAH         POINT TO LAST DISPLAYING FLD                 
*                                                                               
VR30     CR    R2,R3               END OF PRD INPUT FLDS?                       
         BH    VR50                                                             
         CLI   5(R2),0             CURRENT PRD FLD HAS INPUT?                   
         BE    VR48                                                             
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKPRDCOD(0),8(R2)                                                
         OC    WKPRDCOD,SPACES     SPACE PADDED                                 
         BRAS  RE,VR_GETPR         NEED TO DETERMINE OAN SWITCH                 
         MVC   KEY,SVWORK                                                       
         LA    RE,KEY                                                           
         USING PESTKEY,RE                                                       
         MVC   PESTKPRD,WKPRDCOD   PRD CODE TO BE COPIED TO NEW EST             
         DROP  RE                                                               
         CLI   WKOANSW,1           OAN PRODUCT?                                 
         BNE   *+8                                                              
         OI    PESTTEST,X'80'      MUST SET TO TEST EST                         
         L     RE,AIO                                                           
         MVC   0(25,RE),KEY        COPIED EST KEY IN REC (FOR FILE)             
         GOTO1 ADDREC              EST REC IS COPIED                            
         MVC   PESTTEST,WKESTTST   RESTORE TEST STATUS FOR NEXT EST             
*                                                                               
* ADD EMPTY BUCKET REC AND PREPARE AUTOREQ FOR P41                              
*                                                                               
         L     RE,AIO                                                           
         MVC   SVWORK(25),0(RE)    COPIED EST KEY WITH NEW PRD CODE             
         XC    KEY,KEY                                                          
         MVC   KEY(25),SVWORK      PREPARE KEY FOR EST BUCKET REC               
         LA    RE,KEY                                                           
         USING PESTKEY,RE                                                       
         MVI   PESTKRCD,X'09'      EST BUCKET REC CODE                          
         MVI   PESTLEN+0,00                                                     
         MVI   PESTLEN+1,33        REC LENGTH (EMPTY REC)                       
         DROP  RE                                                               
         L     RE,AIO                                                           
         MVC   FULL+0(2),25(RE)    SAVE REC LENGTH                              
         MVC   0(25+2,RE),KEY      EST BUCKET KEY IN REC (FOR FILE)             
         MVC   FULL+2(2),33(RE)    SAVE 1ST ELEM CODE AND LENGTH                
         XC    33(2,RE),33(RE)     FOR ADDING "EMPTY" EST BUCKET REC            
         GOTO1 ADDREC              EST BUCKET REC IS ADDED                      
         L     RE,AIO                                                           
         MVC   25(2,RE),FULL+0     RESTORE ORIGINAL REC LENGTH                  
         MVC   33(2,RE),FULL+2     RESTORE BLANKED OUT BYTES                    
         MVC   KEY,SVWORK          RESTORE EST KEY FROM EST BUCKET KEY          
*                                                                               
         XC    QCTL,QCTL           SET REQ CONTROL BLOCK                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA+00(02),=C'41'                                              
         MVC   QAREA+02(02),AGENCY                                              
         MVC   QAREA+04(01),QMED                                                
         MVC   QAREA+05(03),QCLT                                                
         SR    R0,R0                                                            
         ICM   R0,3,BEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+07,X'0F'                                                     
         UNPK  QAREA+20(03),DUB     CHAR EST WITH LEADING ZEROS                 
         MVC   QAREA+11(03),8(R2)   CURRENT PRD CODE (FROM SCR)                 
         OC    QAREA+11(03),SPACES  SPACE PADDED PRD CODE                       
*                                                                               
         MVC   QAREA+68(07),=C'AUTOREQ'                                         
         MVI   QCTL+10,041                                                      
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
         TM    DMCB+8,X'FF'                                                     
         JNZ   EBREQERR                                                         
*                                                                               
VR48     BRAS  RE,DR_BFLD                                                       
         B     VR30                                                             
*                                                                               
VR50     DS    0H                  FOR FUTURE USES                              
*                                                                               
VRX      DS    0H                                                               
         J     DR                  REDISPLAY VALIDATED RECORD                   
*                                                                               
* VALIDATE PRD CODE TO BE COPIED TO A NEW EST                                   
* EST KEY IS SAVED IN SVWORK                                                    
*                                                                               
VR_GETPR DS    0H                                                               
         ST    RE,FULL                                                          
         MVI   WKOANSW,0                                                        
         XC    KEY+07(L'KEY-07),KEY+07                                          
         MVI   KEY+03,X'06'                                                     
         MVC   KEY+07(L'WKPRDCOD),WKPRDCOD                                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     PRD CODE ON FILE?                            
         JNE   RECNFERR                                                         
         MVC   DUB+04(L'AIO),AIO   SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO2             POINT TO PRD REC                             
         LA    RE,33(RE)           POINT TO 1ST PRD ELEM                        
         CLI   0(RE),X'06'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID PRD REC ENCOUNTERED                  
         USING PPRDELEM,RE                                                      
         CLI   PPRDOAN,C' '        OTHER AGY NAME PRESENT?                      
         BNH   VR_GPRX                                                          
         DROP  RE                                                               
         MVI   WKOANSW,1           SET OAN SWITCH ON                            
         XC    KEY,KEY                                                          
         MVC   KEY(12),SVWORK      EST KEY                                      
         MVC   KEY+07(03),=C'ZZZ'  MUST CK IF ZZZ EST IS LIVE                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     ZZZ ESTIMATE FOUND?                          
         BNE   VR_GPRX                                                          
         GOTO1 GETREC                                                           
         L     RE,AIO2             POINT TO EST REC                             
         LA    RE,33(RE)           POINT TO 1ST EST ELEM                        
         CLI   0(RE),X'07'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID EST REC ENCOUNTERED                  
         TM    PESTTEST-PESTELEM(RE),X'80'                                      
         JZ    OANZZERR            OAN PRD ZZZ'S EST MUST BE TEST               
*                                                                               
VR_GPRX  MVC   AIO,DUB+04          RESTORE ORIGINAL AIO POINTER                 
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         LA    R2,ECPDSP1H         POINT TO FIRST DISPLAYING FLD                
         LA    R3,ECPDSPXH         POINT TO LAST DISPLYING FLD                  
*                                                                               
DR12     CR    R2,R3               END OF LAST DISPLAYING FLD?                  
         BH    DR20                                                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         TM    1(R2),X'02'         EXTENDED FLD HEADER PRESENT?                 
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         AHI   RE,-8-1             SET FOR EX INSTRUCTION                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLR FLD                                      
         OI    6(R2),X'80'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE               PINT TO NEXT FLD                             
         B     DR12                                                             
*                                                                               
* BUILD TABLE OF PRD(S) THAT ARE OPEN, MAX IS 500 ENTRIES                       
* EACH ENTRY IS 4 BYTES, 3 FOR PRD CODE AND 1 FOR "OPEN" INDICATOR              
*                                                                               
DR20     L     R0,AIO3             USE IT TO STORE TABLE OF PRD(S)              
         LHI   R1,2000             MAX OF 500 PRDS IN TABLE (4X500)             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         SR    R3,R3               LOOP COUNTER                                 
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+07(L'KEY-07),KEY+07                                          
         L     R6,AIO3                                                          
         MVI   KEY+03,X'06'        PRD REC CODE                                 
         GOTO1 HIGH                                                             
         B     DR22M                                                            
DR22H    GOTO1 SEQ                                                              
DR22M    CLC   KEY(07),KEYSAVE     PRD ON FILE?                                 
         BNE   DR24                                                             
         MVC   0(3,R6),KEY+07      PLACE PRD CODE IN TABLE                      
         MVI   3(R6),X'FF'         INDICATOR FOR "OPEN"                         
         LA    R6,4(R6)                                                         
         CHI   R3,500              MAX PRD ENTRIES REACHED?                     
         JH    CDADAERR            CANNOT DISP THIS MANY PRD YET                
         AHI   R3,1                                                             
         B     DR22H                                                            
*                                                                               
DR24     L     R6,AIO3             POINT TO TABLE OF PRD CODES                  
DR24H    CLI   0(R6),0             END OF TABLE?                                
         BE    DR28                                                             
         MVC   KEY,SVWORK                                                       
         XC    KEY+12(L'KEY-12),KEY+12                                          
         MVC   KEY+07(03),0(R6)                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     EST ON FILE?                                 
         BE    *+8                                                              
         MVI   3(R6),0             SET INDICATOR TO "NOT OPEN"                  
         LA    R6,4(R6)            POINT TO NEXT ENTRY IN TABLE                 
         B     DR24H                                                            
*                                                                               
DR28     LA    R2,ECPDSP1H         POINT TO 1ST DISPLAYING FLD                  
         MVC   8(21,R2),=C'** Brands not open **'                               
         OI    6(R2),X'80'                                                      
         LHI   R3,13               NUM OF PRD CODES FOR 1ST DISP FLD            
         LA    R4,8+21+7(R2)       POINT TO 1ST DISP AREA                       
         L     R6,AIO3             POINT TO TABLE OF PRD CODES                  
*                                                                               
DR30     CLI   0(R6),0             END OF TABLE?                                
         BE    DR32                                                             
         CLI   3(R6),0             PRD IS NOT OPEN?                             
         BNE   *+8                                                              
         BRAS  RE,DR_DSPRD                                                      
         LA    R6,4(R6)            NEXT ENTRY                                   
         B     DR30                                                             
*                                                                               
DR32     BRAS  RE,DR_BFLD          POINT TO NEXT LINE                           
         BRAS  RE,DR_BFLD          POINT TO NEXT LINE AGAIN                     
         LA    R3,ECPDSPXH         LAST DISPLAYING FLD                          
         CR    R2,R3                                                            
         JH    CDADAERR            MORE TO DISP, BUT SCR IS FULL                
*                                                                               
         MVC   8(25,R2),=C'** Brands already open **'                           
         OI    6(R2),X'80'                                                      
         LHI   R3,13               NUM OF PRD CODES FOR 1ST DISP FLD            
         LA    R4,8+25+3(R2)                                                    
         L     R6,AIO3             POINT TO TABLE OF PRD CODE(S)                
*                                                                               
DR34     CLI   0(R6),0             END OF TABLE?                                
         BE    DR36                                                             
         CLI   3(R6),0             PRD IS OPEN (X'FF'=OPEN)?                    
         BE    *+8                                                              
         BRAS  RE,DR_DSPRD                                                      
         LA    R6,4(R6)            NEXT ENTRY                                   
         B     DR34                                                             
*                                                                               
DR36     LA    R2,ECPPRD1H         1ST PRD INPUT FLD                            
         LA    R3,ECPPRDAH         LAST PRD INPUT FLD                           
DR36H    CR    R2,R3                                                            
         BH    DR38                DONE CLEARING PRD INPUT FLDS                 
         XC    8(L'ECPPRD1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BRAS  RE,DR_BFLD                                                       
         B     DR36H                                                            
*                                                                               
DR38     DS    0H                  FOR FUTURE FLDS                              
*                                                                               
DRX      DS    0H                                                               
         OI    ECPPRD1H+6,X'40'    POSITION CURSOR ON 1ST PRD INPUT FLD         
         J     EXIT                                                             
*                                                                               
* R2 = DISPLAYING FLD POINTER (79 BYTES OF PROTECTED LINES)                     
* R3 = NUMBER OF PRD CODES PER LINE (1ST LINE FITS 13, ALL OTHERS 20)           
* R4 = POINTING TO CURRENT DISPLAYING FLD                                       
* R6 = POINTING TO PRD CODE (IN TABLE) TO BE DISPLAYED                          
*                                                                               
DR_DSPRD DS    0H                  DISPLAY PRD CODES FROM TABLE                 
         CHI   R3,0                                                             
         BNE   DR_DSP5                                                          
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,ECPDSPXH         ADDRESS OF LAST DISPLAYING FLD               
         CR    R2,R0               END OF DISPLAYING FLDS?                      
         JH    CDADAERR            MORE TO DISP, BUT SCR IS FULL                
         LA    R4,8(R2)            POINT TO DISPLAYING FLD AREA                 
         LHI   R3,20               NEW LINE CAN HOLD UP TO 20 PRD CODES         
*                                                                               
DR_DSP5  MVC   0(3,R4),0(R6)       PUT PRD CODE IN DISPLAY FLD                  
         LA    R4,4(R4)            NEXT ENTRY IN DISPLAY FLD                    
         AHI   R3,-1               ONE PRD CODE IS DISPLAYED                    
         OI    6(R2),X'80'                                                      
         BR    RE                                                               
*                                                                               
DR_BFLD  DS    0H                  R2 = FLD POINTER                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST CHANGED                          
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         BE    PPTRS_X                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS_X  B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,001                                                        
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,002                                                        
         J     TRAPERR                                                          
*                                                                               
NTNUMERR MVI   ERROR,003           NOT VALID NUMERIC DATA                       
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
DUPLRERR MVI   ERROR,052           DUPLICATE KEY ON ADD                         
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,053           RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
RECDLERR MVI   ERROR,056           RECORD IS DELETED                            
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,062           INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,068           INVALID DATE FORMAT                          
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,085           SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,089           CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
MAXLNERR MVI   ERROR,090           MAXIMUM RECORD SIZE EXCEEDED                 
         J     TRAPERR                                                          
*                                                                               
CCZZZERR MVI   ERROR,116           CANNOT COPY ZZZ PRD                          
         J     TRAPERR                                                          
*                                                                               
CDADAERR MVI   ERROR,117           CANNOT DISPLAY ADDITIONAL DATA               
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
OANZZERR MVI   ERROR,118           EST FOR OAN ZZZ PRD IS NOT TEST              
         J     TRAPERR                                                          
*                                                                               
EBREQERR MVI   ERROR,120           CANNOT ADD AUTOREQ FOR P41                   
         J     TRAPERR                                                          
*                                                                               
DUPMSG   DC    C'DUPLICATES PRIOR ENTRY - ERASE AND PRESS ENTER'                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    R3,KEY                                                           
         CLI   3(R3),X'07'         EST REC CODE?                                
         BNE   INITI50                                                          
         USING PESTKEY,R3                                                       
         LA    R2,ECPMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PESTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ECPCLTH          CLT FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PESTKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ECPPRDH          PRD FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTPRDH          POINT TO PRD FLD ON LIST SCR                 
         MVC   8(3,R2),PESTKPRD                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ECPESTH          EST FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTESTH          POINT TO EST FLD ON LIST SCR                 
         EDIT  (B2,PESTKEST),(3,8(R2)),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R3                                                               
*                                                                               
INITI50  MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, EST MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,3             PF3, CLT MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,4             PF4, PRD MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,7             PF7, EST LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,8             PF8, EST BILL (ESBILL) MAINT?                
         BE    CKPFK10                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             REC IS EST (FOR MAINT)?                      
         BNE   CKPFK11                                                          
CKPFK10H MVC   DUB,=C'ESTIMATE'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK11  CLI   PFAID,3             REC IS CLT (FOR MAINT)?                      
         BNE   CKPFK12                                                          
CKPFK11H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK12  CLI   PFAID,4             REC IS PRD (FOR MAINT)?                      
         BNE   CKPFK13                                                          
CKPFK12H MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK13  CLI   PFAID,5             REC IS CLT (FOR LIST)?                       
         BNE   CKPFK14                                                          
         B     CKPFK11H                                                         
*                                                                               
CKPFK14  CLI   PFAID,6             REC IS PRD (FOR LIST)?                       
         BNE   CKPFK15                                                          
         B     CKPFK12H                                                         
*                                                                               
CKPFK15  CLI   PFAID,7             REC IS EST (FOR LIST)?                       
         BNE   CKPFK16                                                          
         B     CKPFK10H                                                         
*                                                                               
CKPFK16  CLI   PFAID,8             REC IS EST BILLING (ESBILL) MAINT?           
         BNE   CKPFK17                                                          
         MVC   DUB,=C'ESBILL  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK17  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             EST MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,3             CLT MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,4             PRD MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,8             EST BILL MAINT?                              
         BE    CKPFK28H                                                         
*                                                                               
         B     CKPFK30             CK OTHER PFKEYS FOR LIST                     
*                                                                               
CKPFK28H MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLT LIST?                                    
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK32                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK32  CLI   PFAID,7             EST LIST?                                    
         BNE   CKPFK33                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK33  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ECPMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ECPMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ECPCLTH,,GLVPRCLT   CLIENT                
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ECPPRDH,,GLVPRPRD   PRODUCT               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ECPESTH,,GLVPREST   ESTIMATE              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA+00(2),=C'41'                                               
         MVC   QAREA+02(2),AGENCY                                               
         MVC   QAREA+04(1),QMED                                                 
         MVC   QAREA+05(3),QCLT                                                 
         MVC   QAREA+11(3),QPRD                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
*                                                                               
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME0D          ESTIMATE COPY SCREEN                         
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA4D          ESTIMATE LIST SCREEN                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
WKOANSW  DS    X                   SET TO X'01' IF OAN PRESENT IN PRD           
WKPRDCOD DS    CL3                 PRD CODE ON INPUT SCR TO BE PROC'D           
WKESTTST DS    CL(L'PESTTEST)      TO SAVE PESTTEST                             
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPRDREC           PRD REC DSECT                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE PESTREC           EST REC DSECT                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PRSFM2D   11/10/08'                                      
         END                                                                    
