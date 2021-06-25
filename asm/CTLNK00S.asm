*          DATA SET CTLNK00S   AT LEVEL 013 AS OF 07/25/00                      
*PHASE TA2200A                                                                  
         SPACE 2                                                                
         TITLE 'TA2200 - GENERAL FALINK EMULATOR - BASE'                        
TA2200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GENOLDX-GENOLD,TA2200,RR=R8                                      
*                                                                               
         LA    R7,2048(RC)         RC & R7 BASE REGS FOR                        
         LA    R7,2048(R7)         WORKING STORAGE                              
         USING GENOLD,RC,R7                                                     
         ST    R8,RELO                                                          
         ST    R1,SYSPARMS         SAVE R1 FOR FUTURE REF                       
*                                                                               
         LA    R9,2048(RB)         ESTABLISH R9 AS ANOTHER                      
         LA    R9,2048(R9)         BASE REG                                     
         USING TA2200+4096,R9                                                   
*                                                                               
         L     RA,20(R1)           RA - BASE REG FOR TWA                        
         ST    RA,VTWA                                                          
         USING TA22FFD,RA                                                       
         MVC   AGYALPHA,14(RA)                                                  
*                                                                               
         MVC   VCOMFACS,12(R1)     SET COMFACS ADDRESS                          
*                                                                               
         L     RF,28(R1)                                                        
         ST    RF,VTIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY                                                         
         DROP  RF                                                               
*                                                                               
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
*                                                                               
         OI    LNKSRVH+1,X'01'     TURN MODIFY BIT ON TO GET CONTROL            
         OI    LNKSRVH+6,X'80'                                                  
         OI    LNKPHSH+6,X'40'     POSITION CURSOR ON PHASE FLD                 
         OI    LNKPHSH+6,X'80'                                                  
*                                                                               
         MVI   SPACES,C' '         FILL IN SPASES FIELD                         
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVC   TABCATCH,=C'MYTAB'  SET EYECATCHER FOR HDRTAB                    
*                                                                               
         L     RF,VCOMFACS         GET ADDRESSES OF SUBS                        
         USING COMFACSD,RF         FROM COMFACS                                 
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VXSORT,CXSORT                                                    
         DROP  RF                                                               
*                                                                               
         CLI   16(RA),0                                                         
         BNE   REGMES                                                           
         MVI   SCRBYTE,X'FF'       SET SCR INDICATOR                            
         XC    FLAG,FLAG                                                        
         MVC   SAVEPHS,LNKPHS                                                   
         MVI   16(RA),1                                                         
REGMES   MVC   LNKMSG,REGMSG       OUTPUT REGULAR MSG                           
         OI    LNKMSGH+6,X'80'     EACH TIME USER HITS ENTER                    
*                                                                               
****************THE ACTUAL CODE STARTS HERE*********************                
*                                                                               
*                                                                               
         CLI   PFKEY,7                                                          
         BE    PF7                 GO TO FF SCREEN                              
*                                                                               
         CLI   SCRBYTE,X'FD'                                                    
         BNE   NXTST                                                            
         MVC   LNKMSG,ENDMSG1      YOU'RE DONE                                  
         OI    LNKMSGH+6,X'80'                                                  
         OI    STR1STH+6,X'40'     POSITION CURSOR ON PHASE FLD                 
         OI    STR1STH+6,X'80'     OTHERWISE IT'LL GO TO SERV REQ FIELD         
         B     XIT                                                              
*                                                                               
NXTST    CLC   SAVEPHS,LNKPHS                                                   
         BE    *+12                                                             
         OI    FLAG,X'04'                                                       
         B     PF7                                                              
*                                                                               
*                                                                               
LOADPHS  MVC   FULL,LNKPHS         GET USER ENTERED PHASE                       
         CLI   FULL,C'T'                                                        
         BE    *+18                                                             
         MVC   LNKMSG,WARNMSG      WARNING, 1ST LETTER IS NOT 'T'               
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
         MVI   FULL,0                                                           
         GOTO1 VHEXIN,DMCB,FULL,HALF,L'FULL                                     
*                                                                               
*                                                                               
* CHECK IF THE PHASE NUMBER EXISTS                                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTPHRECD,R4                                                      
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVC   CTPHHEXN(2),HALF                                                 
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,IO                                                            
         CLC   KEY,CTPHPKEY                                                     
         BE    NOER                                                             
         MVC   LNKMSG,NOPHSMS      WARNING, 1ST LETTER IS NOT 'T'               
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NOER     XC    DMCB(24),DMCB       LOAD THAT PHASE IN THE END OF                
         LA    R0,PHSADDR          MY CODE                                      
         ST    R0,DMCB                                                          
         ST    R0,BOPHS                                                         
         MVI   DMCB+4,C'R'                                                      
         MVC   DMCB+5(2),HALF                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB+8         GET LENTH OF PHASE                           
         NI    FULL,0              TURN OFF PHASE INDICATOR                     
         L     R2,BOPHS            GET ADDR OF PHASE END                        
         A     R2,FULL                                                          
         ST    R2,EOPHS                                                         
*                                                                               
*                                                                               
         CLI   LNKSHRTH+5,0         CHECK IF THE USER WANTS ALL                 
         BE    FLGOFF               DESCRIPTIONS TO APPEAR OR ONLY              
         CLI   LNKSHRT,C'N'         THOSE WITH LENGHT OF 14                     
         BE    FLGOFF                                                           
         CLI   LNKSHRT,C'Y'                                                     
         BE    FLGON                                                            
         MVC   LNKMSG,ERRORMSG                                                  
         OI    LNKMSGH+6,X'80'                                                  
         OI    LNKSHRTH+6,X'40'                                                 
         OI    LNKSHRTH+6,X'80'                                                 
         B     XIT                                                              
FLGOFF   NI    FLAG,X'FF'-X'08'                                                 
         B     NXTSTEP                                                          
FLGON    OI    FLAG,X'08'                                                       
         B     NXTSTEP                                                          
*                                                                               
         USING HDRTABD,R3                                                       
NXTSTEP  CLI   PFKEY,6            IF PF6 WAS HIT, GOTO DIFFR ROUTINE            
         BE    PF6                                                              
         CLI   PFKEY,8            IF PF8 WAS HIT                                
         BNE   FFCK                                                             
         TM    FLAG,X'20'         CHECK IS HEADER DESCRIPTIONS FIT              
         BZ    PF6                ON ONE SCREEN, IF NOT, DON'T ALLOW            
         MVC   LNKMSG,MSG6        TO HIT PF8, UNTILL ALL DESCR ARE OUT          
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
*                                                                               
FFCK     CLI   SCRBYTE,X'FE'       IF NO PFKEY HIT & FE SCREEN, EXIT            
         BE    XIT                                                              
         CLI   SCRBYTE,X'FF'       AT THIS POINT CAN ONLY BE FF SCREEN          
         BNE   XIT                                                              
*                                                                               
********* FF SCREEN PROCESSING *****************                                
*                                                                               
         XC    HDRTAB(EOHDRTB),HDRTAB  FILL MY TABLE W/ ZEROS                   
*        BAS   RE,CLEARHDR                                                      
*                                                                               
         BAS   RE,FAMAP           FIND FAMAP & HEADERS                          
         BNE   XIT                                                              
*                                                                               
*   **** OUTPUTTING HEADER CODES ****                                           
*                                                                               
         LA    R3,HDRTAB                                                        
         LA    R2,LNKHDR0H                                                      
*                                                                               
PNXTHDR  GOTO1 VHEXOUT,DMCB,HDRCOD,8(R2),L'HDRCOD                               
         OI    6(R2),X'80'                                                      
         LA    R3,HDRTBLQ(R3)                                                   
         CLI   0(R3),C'#'                                                       
         BE    OUT                                                              
         LA    R1,3              BUMP UP 3 TIMES TO GET TO                      
NXTFLD   ZIC   R0,0(R2)          NXT HDR FIELD                                  
         AR    R2,R0                                                            
         BCT   R1,NXTFLD                                                        
         B     PNXTHDR                                                          
*                                                                               
OUT      DS    0H                                                               
*                                                                               
         BAS   RE,FINTAB         FILL OUT #OF HDRS & ORDER # IN MYTAB           
         BNE   XIT                                                              
         CLI   PFKEY,5           IF PF5 HIT, DIFFR SCREEN                       
         BE    PF5                                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*************** PF5 SCREEN CHANGE ***************                               
*                                                                               
PF5      DS    0H                                                               
         CLI   SCRBYTE,X'FF'       PF5 IS HIT TO CNG FF SCR TO FE               
         BNE   XIT                                                              
*                                                                               
         MVI   SCRBYTE,X'FE'       SCREEN CHANGED                               
         NI    FLAG,X'FF'-X'80'                                                 
         ZIC   R5,NOHDRS           SORT HDR TABLE BY ORDER                      
         GOTO1 VXSORT,DMCB,(0,HDRTAB),(R5),HDRTBLQ,L'HDRORDR,DISPORD            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,LNKLOADH         LOAD FE SCREEN BELOW PHASE NAME              
         ST    RE,DMCB                                                          
         MVI   DMCB,X'FE'                                                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,VTWA             SMTH THAT RETRANSMITTS THE SCREEN            
         LA    R2,64(R2)                                                        
         CLI   0(R2),0                                                          
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         OI    ELMINPH+6,X'40'     POSITION CURSOR ON 1ST INP FLD               
         OI    ELMINPH+6,X'80'                                                  
*                                                                               
GETHDRIN DS    0H                                                               
         LA    R3,HDRTAB           POINT TO MY TABLE                            
FNUM     CLI   HDRNUM,0            IF HDR NUMB NOT 0, PRT DESCRIPTION           
         BNE   PHDR                                                             
         LA    R3,HDRTBLQ(R3)      IF 0, FIND 1ST WHICH IS NOT 0                
         CLI   0(R3),C'#'                                                       
         BNE   FNUM                                                             
         MVC   LNKMSG,ERMSG3       NO HDRS SELECTED                             
         OI    LNKMSGH+6,X'80'                                                  
         XC    SCRBYTE,SCRBYTE                                                  
         MVC   ELMHDR,=C'0000'                                                  
         OI    ELMHDRH+6,X'80'                                                  
         B     XIT                                                              
PHDR     SR    R3,RA               SAVE DISPL TO MYTAB                          
         ST    R3,HDTABPTR                                                      
*                                                                               
         LA    R5,STRING           CLEAR STRING TO TRANSMITT BLANKS             
         XC    0(256,R5),0(R5)     AFTER END OF DATA                            
         XC    255(256,R5),255(R5)                                              
         XC    510(256,R5),510(R5)                                              
         XC    765(256,R5),765(R5)                                              
         XC    1020(256,R5),1020(R5)                                            
         XC    1275(256,R5),1275(R5)                                            
         XC    1530(230,R5),1530(R5)                                            
*                                                                               
         LA    R5,STRING           SAVE DISPL TO STRING                         
         MVC   0(4,R5),=C'UEO '                                                 
         LA    R5,4(R5)                                                         
         SR    R5,RA                                                            
         ST    R5,DISPSTR                                                       
*                                                                               
         BAS   RE,PHDDESC          OUTP DESCR OF FRST HDR                       
         TM    FLAG,X'20'                                                       
         BZ    XIT                                                              
         MVC   LNKMSG,MSG3         NO HDRS SELECTED                             
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
********* PF6 SCREEN CHANGE **********                                          
*                                                                               
PF6      DS    0H                  PF6 IS ONLY FOR FE SCREEN                    
         CLI   SCRBYTE,X'FF'       THERE'S NO PF6 ON FF SCREEN                  
         BE    XIT                                                              
*                                                                               
         OI    ELMINPH+6,X'40'     POSITION CURSOR ON 1ST INP FLD               
         OI    ELMINPH+6,X'80'                                                  
         NI    FLAG,X'FF'-X'20'                                                 
*                                                                               
         L     R3,HDTABPTR         POINT R3 TO WHERE I LEFT OF IN MYTAB         
         AR    R3,RA                                                            
*                                                                               
HEXIN    GOTO1 VHEXIN,DMCB,ELMHDR,HALF,L'ELMHDR                                 
         CLC   HALF,HDRCOD         COMPARE HDR CODE FROM SCREEN                 
         BE    *+18                WITH CURRENT HDR CODE IN MY TABLE            
         MVC   LNKMSG,ERMSG1       HEADER MISMATCH                              
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                 IF NOT =, ERROR                              
*                                                                               
         LA    R2,ELMDESCH         POINT R2 TO 1ST DESCRIPTION FLD              
         LA    R6,ELMLDSCH         --"-- R6 TO LAST --- " ---                   
         L     R5,DISPSTR          POINT R5 TO NXT AVAIL BYTE IN STRING         
         AR    R5,RA                                                            
*        OC    FLAG,FLAG           IF FLAG IS EMPTY, NEW HDR                    
         TM    FLAG,X'80'          IF FLAG IS EMPTY, NEW HDR                    
         BZ    NEWHDR                                                           
         L     R4,DISPHDEL         IF NOT, LOAD PTR TO NXT DESCRIPT,            
         A     R4,BOPHS            WHICH DID NOT FIT ON ONE SCREEN              
         B     PF610                                                            
NEWHDR   TM    FLAG,X'40'                                                       
         BNZ   TST1                                                             
         MVC   0(2,R5),=C'B='      MOVE CONSTANTS IN                            
         LA    R5,2(R5)                                                         
         GOTO1 VHEXOUT,DMCB,HDRCOD+1,HALF,1                                     
         MVC   0(2,R5),HALF        MOVE HDR CODE TO STRING                      
         LA    R5,2(R5)                                                         
*                                                                               
TST1     L     R4,BOPHS            POINT TO BEGIN OF LOADED PHASE               
         MVC   HALF,HDRDISP                                                     
         AH    R4,HALF             R4 IS POINTING TO HEADER                     
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         USING HDRELEMD,R4                                                      
*                                                                               
*                                                                               
PF610    CLI   ELLEN,0             END OF HEADER?                               
         BNE   *+12                                                             
         NI    FLAG,X'FF'-X'80'    IF YES, RESET FLAG TO INDIC NEW HDR          
         B     SCRDONE                                                          
         CR    R2,R6               PAST THE LAST SCREEN FIELD?                  
         BH    SETFLG              IF YES, SET FLAG TO INDIC MORE DESCR         
         TM    FLAG,X'08'                                                       
         BNZ   *+12                                                             
         CLI   ELLEN,14                                                         
         BNE   NXTEL               IF ELEM LEN IS NOT 14, DON'T BOTHER          
         CLC   8(2,R2),ELDISC      IS ON-SCREEN DESCRIPT=IN-PHASE DESCR         
         BE    *+18                                                             
         MVC   LNKMSG,ERMSG2       DESCR MISMATCH                               
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
*                                                                               
         ZIC   R0,0(R2)             GET TO USER INPUT FIELD                     
         AR    R2,R0                TO VALIDATE DATA                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    NXTDESC                                                          
*                                                                               
*        CLI   VALBYTE,0           IS THERE ANY IMPL LENGHT?                    
*        BE    NOVAL               IF NOT NO DATA VALIDATION                    
*        CLC   VALBYTE,5(R2)                                                    
*        BNE   INPERROR                                                         
*                                                                               
         CLI   ELTYPE,MDTMDQ                                                    
         BNE   REGSTR                                                           
         MVI   0(R5),C'%'                                                       
         LA    R5,1(R5)                                                         
         B     PUTMPCOD                                                         
*                                                                               
REGSTR   XC    HALF,HALF                                                        
         MVC   HALF+1(1),5(R2)     PUT IN HALF INPUT LENGTH                     
         BAS   RE,TRTLEN           MATCH INPUT LEN WITH TAB OF LENGTHS          
         ZIC   R0,PLSCNT           ARE THERE ANY +'S IN LENGTH?                 
         LTR   R0,R0                                                            
         BZ    PUTCHAR                                                          
PUTPLS   MVI   0(R5),C'+'          PUT +'S IF ANY                               
         LA    R5,1(R5)                                                         
         BCT   R0,PUTPLS                                                        
PUTCHAR  MVC   0(1,R5),LEN         PUT LENGTH                                   
         LA    R5,1(R5)                                                         
*                                                                               
PUTMPCOD MVC   HALF,ELCODE         PUT MAP CODE IN HALF                         
         BAS   RE,TRTLEN           TRANSLATE INTO LENGTH                        
         ZIC   R0,PLSCNT                                                        
         LTR   R0,R0                                                            
         BZ    PCHAR                                                            
PPLUS    MVI   0(R5),C'+'                                                       
         LA    R5,1(R5)                                                         
         BCT   R0,PPLUS                                                         
PCHAR    MVC   0(1,R5),LEN                                                      
         LA    R5,1(R5)                                                         
*                                                                               
         CLI   ELTYPE,9                                                         
         BE    NXTDESC                                                          
*                                                                               
         ZIC   R1,5(R2)             GET INP LEN FOR EX MOVE                     
         LTR   R1,R1                IF NOTHING ENTERED, GOTO NXT FIELD          
         BZ    NXTDESC                                                          
         BCTR  R1,R0                                                            
         EX    R1,*+8               MOVE INPUT INTO STRING                      
         B     *+10                                                             
         MVC   0(0,R5),8(R2)                                                    
         AHI   R1,1                 RESTORE INP LENGTH                          
         AR    R5,R1                ADJUST STRING POINTER BY INP LEN            
*                                                                               
NXTDESC  ZIC   R0,0(R2)             GET TO NXT DESCR FIELD ON SCREEN            
         AR    R2,R0                                                            
NXTEL    ZIC   R0,ELLEN             GET TO NXT DESCR FIELD IN PHASE             
         AR    R4,R0                                                            
         B     PF610                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
SETFLG   OI    FLAG,X'80'                                                       
         MVC   LNKMSG,OVFLMSG       DISPL MSG THAT THERE'S MORE DESCR           
         OI    LNKMSGH+6,X'80'                                                  
         S     R4,BOPHS             IF DESCR DIDN'T FIT REMEMBER WHERE          
         ST    R4,DISPHDEL          IT STOPED                                   
*                                                                               
SCRDONE  DS    0H                                                               
         SR    R5,RA                SAVE DISPL WHERE LEFT OF IN STRING          
         ST    R5,DISPSTR                                                       
*                                                                               
         BAS   RE,CLEARSCR          CLEAR ALL FIELS OF THE PREV SCREEN          
*                                                                               
         CLI   PFKEY,8                                                          
         BNE   *+12                                                             
         OI    FLAG,X'40'                                                       
         B     CALLSUB                                                          
         NI    FLAG,X'FF'-X'40'                                                 
         TM    FLAG,X'80'                                                       
         BO    CALLSUB                                                          
*                                                                               
         ZIC   R0,HDRNUM           DECREMENT HDR NUMBER                         
         BCTR  R0,R0               IN CASE HEADER RECQUESTED                    
         STC   R0,HDRNUM           MORE THAN 1NCE                               
FNDNUM   CLI   HDRNUM,0            IF NUMB=0, GOTO NXT TAB ENTRY                
         BNE   PF620                                                            
         LA    R3,HDRTBLQ(R3)                                                   
         CLI   0(R3),C'#'          CHECK FOR END OF HDR TABLE                   
         BNE   FNDNUM                                                           
         L     R5,DISPSTR          IF EOT, MARK END OF STRING                   
         AR    R5,RA                                                            
         MVI   0(R5),X'FE'                                                      
         MVC   LNKMSG,ENDMSG                                                    
         OI    LNKMSGH+6,X'80'                                                  
         MVI   SCRBYTE,X'FD'                                                    
         B     LSTSCR                                                           
*                                                                               
PF620    SR    R3,RA                                                            
         ST    R3,HDTABPTR         SAVE DISPL TO HDR TAB ENTRY                  
*                                                                               
CALLSUB  BAS   RE,PHDDESC          DISPL DESCR OF NXT HDR OR                    
*        OC    FLAG3,FLAG3                                                      
         TM    FLAG,X'20'                                                       
         BZ    XIT                                                              
         MVC   LNKMSG,MSG3         NO HDRS SELECTED                             
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                 WHAT DIDN'T FIT ON FULL SCREEN               
         EJECT                     FOR THE SAME HEADER                          
*                                                                               
*********LAST SCREEN************************************8                       
*                                                                               
LSTSCR   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,LNKFDSCH                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,X'FD'                                                       
         GOTO1 VCALLOV,DMCB        LOAD SCREEN WITH STRINGS                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,VTWA             SMTH THAT RETRANSMITTS THE SCREEN            
         LA    R2,64(R2)                                                        
         CLI   0(R2),0                                                          
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         BAS   RE,CLRFDSCR                                                      
         LA    R2,STR1STH          POINT TO 1ST FIELD                           
         LA    R6,STRLAST          POINT TO LAST SCREEN                         
         LA    R5,STRING                                                        
*                                                                               
PSTR     CR    R2,R6                WHEN PAST LAST FIELD XIT                    
         BNL   XIT                                                              
         MVC   8(79,R2),0(R5)       PUT FROM STRING TO SCREEN                   
         ZIC   R0,0(R2)             GET TO NXT FIELD                            
         AR    R2,R0                                                            
         LA    R5,79(R5)            POINT TO NXT 79 CHAR'S IN STRING            
         B     PSTR                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
********** PF7 SCREEN CHANGE ***********************                            
*                                                                               
PF7      DS    0H                 GO BACK TO FF  SCREEN                         
         MVI   SCRBYTE,X'FF'                                                    
         MVC   SAVEPHS,LNKPHS                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    RE,LNKMSGH                                                       
         ST    RE,DMCB                                                          
         MVI   DMCB,X'FF'                                                       
         GOTO1 VCALLOV,DMCB        LOAD FF SCREEN                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,VTWA             SMTH THAT RETRANSMITTS THE SCREEN            
         LA    R2,64(R2)                                                        
         CLI   0(R2),0                                                          
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'                                                 
*                                                                               
         TM    FLAG,X'04'                                                       
         BZ    FLXC                                                             
*        MVI   LNKPHS,C'T'                                                      
         MVC   LNKPHS,SAVEPHS                                                   
         OI    LNKPHSH+6,X'80'                                                  
         XC    FLAG,FLAG                                                        
         OI    LNKSRVH+1,X'01'     TURN MODIFY BIT ON TO GET CONTROL            
         OI    LNKSRVH+6,X'80'                                                  
         MVC   LNKMSG,REGMSG       OUTPUT REGULAR MSG                           
         OI    LNKMSGH+6,X'80'                                                  
         B     LOADPHS                                                          
*                                                                               
FLXC     XC    FLAG,FLAG                                                        
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
************************** SUBROUTINES *****************************            
*                                                                               
*                                                                               
*                                                                               
*********************************************************************           
* FAMAP WILL FIND EYECATCHER, COUNT # OF HEADERS IN PHASE & BIULD   *           
* A TABLE OF HEADER CODES AND DISPL OF EACH HDR FROM BEGIN OF PHASE *           
*********************************************************************           
FAMAP    NTR1                                                                   
         L     R2,BOPHS                                                         
FINDMAP  CLC   =C'**FAMAP*',0(R2)  CHECK FOR EYECATCH. WHICH IS                 
         BE    GOTMAP                                                           
         LA    R2,8(R2)            ON DOUBLE WORD BOUNDRY                       
         C     R2,EOPHS                                                         
         BL    FINDMAP                                                          
         MVC   LNKMSG,ERMSG                                                     
         OI    LNKMSGH+6,X'80'                                                  
         B     XIT                                                              
*                                                                               
GOTMAP   DS    0H                                                               
         SR    R0,R0               HEADER COUNTER                               
         LA    R2,8(R2)            POINT TO HEADER LENGTH                       
         LA    R3,HDRTAB                                                        
         MVI   EOHDRTB(R3),X'FF'   MARK EOT                                     
NXTHDR   MVC   HDRCOD,1(R2)        STORE HDR CODE IN MYTAB                      
         LR    R1,R2                                                            
         S     R1,BOPHS            A(HEADER)-A(PHASE BEGIN)                     
         STH   R1,HDRDISP          STORE DISPL IN MYTAB                         
         LA    R3,HDRTBLQ(R3)      GET TO NXT TABLE ROW                         
         AHI   R0,1                INCREMENT HDR COUNTER                        
         CLI   0(R3),X'FF'         IF MORE THAN 40 HDRS DIE                     
         BNE   *+20                                                             
         MVC   LNKMSG,HDOVMSG       DISPL MSG THAT THERE'S MORE DESCR           
         OI    LNKMSGH+6,X'80'                                                  
         CR    RA,RB                                                            
         B     XTSUB                                                            
         LR    R1,R2                                                            
         AH    R2,3(R1)            ADD DISP TO NXT HEADER                       
         CLI   0(R2),0                                                          
         BNE   NXTHDR                                                           
*                                                                               
NOMORE   DS    0H                                                               
         STC   R0,NOHDRS                                                        
         MVI   0(R3),C'#'          MARK END OF MYTAB                            
         CR    RB,RB                                                            
XTSUB    XIT1                                                                   
*                                                                               
*                                                                               
********************************************************************            
* SUB WILL ADD NUMBER OF HEADERS & ORDER REQUESTED TO MY HDR TABLE *            
********************************************************************            
FINTAB   NTR1                                                                   
         LA    R2,LNKNUM0H         POINT TO 1ST NUMBER FIELD                    
         LA    R3,HDRTAB                                                        
*                                                                               
*                                                                               
FILTAB   CLI   5(R2),0                                                          
         BNE   NXTCK               IF SMTH ENTERED, GO CHECK FURTHER            
*                                                                               
         ZIC   R0,0(R2)            BUMP UP 2 TIMES                              
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            TO GET TO ORDER FIELD                        
         AR    R2,R0                                                            
         CLI   5(R2),0             CHECK IF EMPTY                               
         BNE   INPERROR                                                         
PUTFF    MVI   HDRORDR,X'FF'       TO SORT UNSELECTED FLDS LAST                 
         B     NXTHDR2             GET READY TO CHECK NXT NUMB FIELD            
*                                                                               
NXTCK    TM    4(R2),X'08'         CHECK FIELD FOR NUMERIC                      
         BZ    INPERROR                                                         
         ZIC   R1,5(R2)            CONV NUMBER TO BINARY                        
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,HDRNUM           STORE IT IN MY HDR TABLE                     
*                                                                               
         ZIC   R0,0(R2)            BUMP UP 2 TIMES                              
         AR    R2,R0               TO GET TO ORDER FIELD                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             DO THE SAME FOR ORDER                        
         BE    PUTFF                                                            
         TM    4(R2),X'08'                                                      
         BZ    INPERROR                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,HDRORDR                                                       
*                                                                               
NXTHDR2  LA    R3,HDRTBLQ(R3)                                                   
         CLI   0(R3),C'#'           ARE ANY HEADERS LEFT                        
         BE    TABDONE              NO, DONE                                    
         ZIC   R0,0(R2)             YES, GET TO NXT NUMB FIELD                  
         AR    R2,R0                                                            
*                                                                               
         B     FILTAB                                                           
*                                                                               
*                                                                               
TABDONE  DS    0H                                                               
         CR    RB,RB               SET CC TO =                                  
         B     XITSUB                                                           
*                                                                               
INPERROR MVC   LNKMSG,ERRORMSG                                                  
         OI    LNKMSGH+6,X'80'                                                  
         OI    6(R2),X'40'                                                      
         OI    6(R2),X'80'                                                      
         CR    RA,RB               SET CC TO NOT=                               
*                                                                               
XITSUB   XIT1                                                                   
*                                                                               
***************************************************************                 
* SUB WILL OUTPUT HDR DESCRIPTIONS                            *                 
***************************************************************                 
PHDDESC  NTR1                                                                   
         L     R3,HDTABPTR       GET NXT HDR AND OUTPUT IT                      
         AR    R3,RA                                                            
         USING HDRTABD,R3                                                       
         GOTO1 VHEXOUT,DMCB,HDRCOD,ELMHDR,L'HDRCOD                              
         OI    ELMHDRH+6,X'80'                                                  
         LA    R2,ELMDESCH                                                      
         LA    R6,ELMLDSCH                                                      
*                                                                               
*        OC    FLAG,FLAG         IF NO FLAG, NEW HDR                            
         TM    FLAG,X'80'        IF NO FLAG, NEW HDR                            
         BZ    REGULAR                                                          
         TM    FLAG,X'01'        IS IT 3RD PAGE?                                
         BZ    *+14                                                             
         MVC   DISPHDEL,DSPLST   GIVE IT 3RD PAGE'S DISPLACEMENT                
         NI    FLAG,X'FF'-X'01'                                                 
         L     R4,DISPHDEL       IF YES, START DESCR WHERE LEFT OFF             
         A     R4,BOPHS                                                         
         B     DESCOUT                                                          
REGULAR  L     R4,BOPHS            POINT TO BEGIN OF LOADED PHASE               
         MVC   HALF,HDRDISP                                                     
         AH    R4,HALF             R4 IS POINTING TO HEADER                     
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         USING HDRELEMD,R4                                                      
DESCOUT  CLI   ELLEN,0                                                          
         BE    HDRFIN                                                           
         CR    R2,R6                                                            
         BH    OVRFLOW                                                          
         TM    FLAG,X'08'                                                       
         BNZ   *+12                                                             
         CLI   ELLEN,14            IF ELEM LEN IS NOT 14 SKIP                   
         BNE   NXTELEM                                                          
         MVC   8(L'ELDISC,R2),ELDISC   OUTPUT DESCRIPTION                       
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
NXTELEM  ZIC   R0,ELLEN                                                         
         AR    R4,R0                                                            
         B     DESCOUT                                                          
         DROP  R4                                                               
         DROP  R3                                                               
OVRFLOW  OI    FLAG,X'20'                                                       
         S     R4,BOPHS                                                         
         TM    FLAG,X'80'          IS IT EXCEEDING 2ND PAGE?                    
         BZ    *+12                                                             
         ST    R4,DSPLST           YES, KEEP DISPL TO 3RD PAGE OF DESC          
         OI    FLAG,X'01'          AND DON'T TOUCH DESCR FOR 2ND PAGE           
         B     *+8                                                              
         ST    R4,DISPHDEL         (PRETTY STUPID, I HOPE TO CHNGE IT)          
         B     GETOUT                                                           
HDRFIN   CLI ELMINPH+5,0                                                        
         BNE   GETOUT                                                           
         OI    ELMHDRH+6,X'40'                                                  
         OI    ELMHDRH+6,X'80'                                                  
GETOUT   XIT1                                                                   
*                                                                               
*********************************************************                       
* SUB WILL CLEAR FIELDS OF FE SCREEN                                            
*********************************************************                       
CLEARSCR NTR1                                                                   
         LA    R2,ELMDESCH                                                      
         LA    R5,ELMLINF                                                       
*                                                                               
LOOP     CR    R2,R5                                                            
         BH    DONE                                                             
         XC    8(L'ELMDESC,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'ELMINP,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     LOOP                                                             
*                                                                               
DONE     XIT1                                                                   
*                                                                               
*********************************************************                       
*********************************************************                       
* SUB WILL CLEAR FIELDS OF FE SCREEN                                            
*********************************************************                       
CLRFDSCR NTR1                                                                   
         LA    R2,STR1STH                                                       
         LA    R5,STRLSTH                                                       
*                                                                               
LOOPFD   CR    R2,R5                                                            
         BH    DONEFD                                                           
         XC    8(L'ELMDESC,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     LOOPFD                                                           
*                                                                               
DONEFD   XIT1                                                                   
*                                                                               
* SUB WILL CLEAR FIELDS OF FF SCREEN                                            
*********************************************************                       
CLEARHDR NTR1                                                                   
         LA    R2,LNKNUM0H                                                      
         LA    R5,LNKLORDH                                                      
*                                                                               
LOOP1    CR    R2,R5                                                            
         BH    DONE1                                                            
         XC    8(L'LNKNUM0,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'LNKHDR0,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(L'LNKODR0,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     LOOP1                                                            
*                                                                               
DONE1    XIT1                                                                   
*                                                                               
*****************************************************************               
* SUB WILL TRANSLATE NUMBER IN HEX TO NUMB UNDERSTOOD BY FALINK *               
*****************************************************************               
TRTLEN   NTR1                                                                   
         LH    R2,HALF             GET NUMBER                                   
         SR    R0,R0               '+' COUNTER                                  
FIT      CHI   R2,X'3F'                                                         
         BNH   GETLEN                                                           
         AHI   R0,1                 IF HIGHER THEN 3F ADD '+'                   
         SHI   R2,X'3F'             AND SUBTR 3F FROM NUMBER                    
         B     FIT                  CHECK AGAIN                                 
GETLEN   STC   R2,LEN                                                           
         TR    LEN,LENGTHS                                                      
         STC   R0,PLSCNT                                                        
         XIT1                                                                   
***************CONSTANTS*********************************                       
*                                                                               
ERRORMSG DC    CL60'CHECK YOUR INPUT!'                                          
ERMSG    DC    CL60'NO FAMAP FOUND!'                                            
HDOVMSG  DC    CL60'MORE THAN 40 HEADERS FOUND, CAN''T PROCEED'                 
ERMSG1   DC    CL60'HEADER CODE MISMATCH!'                                      
ERMSG2   DC    CL60'DESCRIPTION MISMATCH!'                                      
ERMSG3   DC    CL60'NO HEADERS WERE SELECTED PRESS PF7 TO GO BACK'              
NOPHSMS  DC    CL60'CHECK YOUR PHASE ENTRY'                                     
REGMSG   DC    CL60'PLEASE MAKE YOUR SELECTION'                                 
OVFLMSG  DC    CL60'HERE''S MORE DESCRIPTION TO THE SAME HEADER'                
WARNMSG  DC    CL60'FIRST CHARACTER IN PHASE NAME IS USUALLY "T"'               
MSG6     DC    CL60'THERE''S MORE TO THE SAME HEADER, HIT PF6'                  
MSG3     DC    CL60'HEADER IS TOO LONG DON''T PRESS PF8'                        
ENDMSG   DC    CL60'HERE''S YOUR STRING, ENJOY!'                                
ENDMSG1  DC    CL60'YOU''RE DONE!(*_*) TO START AGAIN PRESS PF7'                
LENGTHS  DC    C'%ABCDEFGHIJKLMNO'                     00-0F                    
         DC    C'PQRSTUVWXYZ01234'                     10-1F                    
         DC    C'56789abcdefghijk'                     20-2F                    
         DC    C'lmnopqrstuvwxyz+'                     30-3F                    
*                                                                               
         LTORG                                                                  
         DC    C'**YOUR PHASE*'                                                 
PHSADDR  DS    0H                  PLACE TO LOAD THE PHASE                      
*                                                                               
         EJECT                                                                  
GENOLD   DSECT                                                                  
RELO     DS    A                                                                
*                                                                               
VTWA     DS    A                                                                
VTIOB    DS    A                                                                
SYSPARMS DS    A                                                                
VDATAMGR DS    A                                                                
VCALLOV  DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VXSORT   DS    A                                                                
VCOMFACS DS    A                                                                
*                                                                               
BASERB   DS    A                                                                
BASERD   DS    A                                                                
FRSTFLD  DS    A                                                                
LASTFLD  DS    A                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
LEN      DS    X                                                                
PLSCNT   DS    X                                                                
AGYALPHA DS    H                                                                
NUMFLD   DS    H                                                                
DMCB     DS    6F                                                               
SPACES   DS    CL80                                                             
WORK     DS    CL64                                                             
KEY      DS    CL25                                                             
IO       DS    1000X                                                            
*                                                                               
*                                                                               
*    SPACE                                                                      
*                                                                               
*                                                                               
*                                                                               
GENOLDX  DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE CTLNKFFD                                                       
         ORG   LNKLOADH                                                         
       ++INCLUDE CTLNKFED                                                       
         ORG   LNKFDSCH                                                         
       ++INCLUDE CTLNKFDD                                                       
*                                                                               
         ORG   LNKMSGH+3500        SAVED STORAGE                                
PFKEY    DS    X                                                                
SCRBYTE  DS    X                                                                
NOHDRS   DS    X                                                                
FLAG     DS    X                                                                
SAVEPHS  DS    F                                                                
BOPHS    DS    F                                                                
EOPHS    DS    F                                                                
DISPSTR  DS    F                                                                
DISPHDEL DS    F                                                                
DSPLST   DS    F                                                                
HDTABPTR DS    F                                                                
TABCATCH DS    CL5                                                              
HDRTAB   DS    240X                                                             
EOHDRTB  EQU   *-HDRTAB                                                         
         DS    X                                                                
STRING   DS    XL1600                                                           
*                                                                               
*                                                                               
HDRTABD  DSECT                                                                  
HDRCOD   DS    XL2                                                              
HDRDISP  DS    XL2                                                              
HDRNUM   DS    X                                                                
DISPORD  EQU   *-HDRTABD                                                        
HDRORDR  DS    X                                                                
HDRTBLQ  EQU   *-HDRTABD                                                        
*                                                                               
HDRELEMD DSECT                                                                  
ELLEN    DS    X                                                                
ELCODE   DS    XL2                                                              
ELDISC   DS    CL5                                                              
ELTYPE   DS    X                                                                
VALBYTE  DS    X                                                                
         DS    XL4                                                              
HDRELLQ  EQU   *-HDRELEMD                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FALINKBLK                                                      
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CTLNK00S  07/25/00'                                      
         END                                                                    
