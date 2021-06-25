*          DATA SET RECNT7A    AT LEVEL 140 AS OF 01/09/13                      
*PHASE T8027AA                                                                  
*&&      SET   TT=N,T1=Y,T2=N                                                   
*INCLUDE REGENBUF                                                               
         TITLE 'T8027A - RECNT7A - CONTRACT EMAILER'                            
*********************************************************************           
*                                                                   *           
*        RECNT7A --- SENDING AN EMAIL NOTIFICATION WHEN SEND ACTION *           
* ----------------------------------------------------------------- *           
* T1=N,T2=Y : DDS TESTING OPTION                                    *           
* T1=Y,T2=N : PRODUCTION OPTION                                     *           
* 15APR02 HQI NEW                                                   *           
* 05DEC02 JRD CHANGE SUBJECT LINE FORMAT TO:                        *           
*                 STAT-B MMMDD/YY-MMMDD/YY ADVERTISER OF UNWIRED    *           
*             REP ORDER COMMENTS                                    *           
* 19FEB03 JRD SUPPORT STANDARD AND STORED COMMENTS.                 *           
* 24JAN05 HQI SUPPRESS EMAIL BASE ON STATION PROFILE                *           
*             SUPPRESS URL FOR STATION ORDER MANAGER                *           
* 03MAY05 HQI CHANGE WORDING ON CONTRACT STATUS                     *           
* 19JAN06 BU  ADD URL FOR 3.0                                       *           
* 30MAR06 BU  ADD URL FOR STX 1.0, ETC                              *           
* 01FEB07 BU  PICK UP URL FROM REP RECORD, IF PRESENT               *           
*  APR12  SMY SWITCH TO JESMAIL                                     *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T8027A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWRKDX-MYWRKD,*T8027A*,RR=R8,CLEAR=YES                          
         LR    R7,RC                                                            
         USING MYWRKD,R7                                                        
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
*****    L     R8,ASPOOLD                                                       
*****    USING SPOOLD,R8                                                        
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   AJESMAIL,CJESMAIL   SAVE JESMAIL ADDRESS                         
         DROP  RF                                                               
         EJECT                                                                  
*********************************************************************           
*        MAIN LINE PROCESSING - SENDS EMAIL NOTIFY TO STATION                   
*********************************************************************           
* TEST   CLC   RCONKCON,=X'3864325'                                             
* TEST   BNE   EMLINFOX                                                         
*                                                                               
         CLI   TWAACCS,C'$'        IF STATION?                                  
         BE    NOEMLER             DO NOT GENERATE EML                          
*                                                                               
         MVC   MASTEREP,RCONKREP                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RREPKEY,KEY                                                      
         MVI   KEY,X'01'            READ REP RECORD                             
         MVC   K.RREPKREP,RCONKREP                                              
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EML010                                                           
*                                                                               
         USING RREPCODE,R6                                                      
         OC    RREPMAST,RREPMAST                                                
         BZ    EML010                                                           
         CLC   RREPMAST,=XL2'4040'                                              
         BE    EML010                                                           
         CLC   RREPMAST,=XL2'FFFF'                                              
         BE    EML010                                                           
*                                                                               
         MVC   MASTEREP,RREPMAST                                                
*                                                                               
EML010   DS    0H                                                               
         MVC   XTRACTY,RREPPROF+11                                              
*                                                                               
         XC    SAVURL,SAVURL       INITIALIZE SAVE AREA                         
         MVI   SAVURLEN,0          CLEAR LENGTH ATTRIBUTE                       
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EML012                                                           
         USING RREPURL#,R6                                                      
         ZIC   RF,1(R6)                                                         
         SH    RF,=H'3'            REDUCE LENGTH FOR EX MOVE                    
         EX    RF,EML011           MOVE BY LENGTH                               
         LA    RF,1(RF)            SAVE ACTUAL LEN OF URL                       
         STC   RF,SAVURLEN         SAVE LENGTH OF URL                           
         B     EML012                                                           
EML011   EQU   *                                                                
         MVC   SAVURL(0),2(R6)     MOVE DATA BY LENGTH                          
         DROP  R6                                                               
*                                                                               
EML012   DS    0H                                                               
         USING RREPCODE,R6                                                      
         L     R4,AIO                                                           
         USING RCONKEY,R4                                                       
K        USING RSTAKEY,KEY         GET STATION RECORD                           
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   K.RSTAKREP,RCONKREP                                              
         MVC   K.RSTAKSTA,RCONKSTA                                              
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
*                                                                               
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         MVI   SVRSTAP6,C'N'       Set new URL profile to no                    
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'25'        EMAIL ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   NOEMLER             NO EMAIL,NO REPORT GENERATED                 
*                                                                               
         LA    R6,RSTAREC                                                       
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,X'01'        Main station element                         
         BRAS  RE,GETEL                                                         
         JNE   *+10                                                             
         MVC   SVRSTAP6,RSTAP6     Save "new URL" profile                       
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        READ BIT 22 FOR EMAIL SETTING                
         BAS   RE,GETEL                                                         
         BNE   NOEMLER                                                          
         USING RSTAXXEL,R6                                                      
*                                                                               
*   SOM 3.0 SUPERCEDES 2.1.  STX 1.X SUPERCEDES BOTH.                           
*                                                                               
         MVI   URL3DOT0,C'N'       SET URL = 2.1                                
         TM    RSTAOPTC,X'02'      STATION SET TO MO 3.0?                       
         BNO   EML015              NO                                           
         MVI   URL3DOT0,C'Y'       YES - SET URL = 3.0                          
EML015   EQU   *                                                                
         TM    RSTAOPTC,X'01'      STATION SET TO STX 1.X?                      
         BNO   EML017              NO                                           
         MVI   URL3DOT0,C'S'       YES - SET URL = STX 1.X                      
EML017   EQU   *                                                                
*                                                                               
         TM    RSTAOPTB,X'08'      STATION CONFIRMS VIA WEB?                    
         BZ    NOEMLER             NO                                           
         TM    RSTAOPTC,X'40'      STATION CHOOSE NOT TO RECEIVE EMAIL?         
         BO    NOEMLER             YES                                          
*                                                                               
         XCEFL EMLFLDS,'EMLFLDLN'  CLEAR                                        
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'25'        EMAIL ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
         USING RSTAEML,R6                                                       
*                                                                               
*                                  PRIMARY RECIPIENT                            
*                                                                               
         ZIC   R5,RSTAEMLN                                                      
         SHI   R5,RSTAADD-RSTAEML                                               
         CHI   R5,1                                                             
         BH    *+6                                                              
         DC    H'0'                NO TO ADDR                                   
*                                                                               
         LA    R2,EMLTOADR                                                      
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RSTAADD                                                  
         MVI   EMLTOEND,X'FF'      TERMINATOR                                   
*                                                                               
         LA    R2,EMLCCADR         POINT TO CC ADDRESS ARRAY                    
*                                                                               
EML020   BRAS  RE,NEXTEL                                                        
         BNE   EML022                                                           
*                                  CC: RECIPIENTS (UP TO THREE)                 
         ZIC   R5,RSTAEMLN                                                      
         SHI   R5,RSTAADD-RSTAEML                                               
*                                                                               
         CHI   R5,1                                                             
         BNH   EML020              EMPTY ELT                                    
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RSTAADD                                                  
*                                                                               
         LA    R2,60(R2)           NEXT ARRAY AREA                              
         MVI   0(R2),X'FF'         TERMINATOR IF NO MORE ADDRESSES              
         B     EML020                                                           
*                                                                               
EML022   DS    0H                                                               
*                                                                               
*                                  BCC:RECIPIENT                                
         MVC   EMLBCADR(L'BCCADR),BCCADR   ****  STXP E LOG  ****               
*                                                                               
         MVI   EMLBCEND,X'FF'      TERMINATOR                                   
*                                                                               
         CLI   SVRSTAP6,C'Y'       Use new URL?                                 
         JNE   EML025                                                           
         XC    SAVURL,SAVURL       Reset URL                                    
         XC    KEY,KEY                                                          
K        USING RURLKEY,KEY                                                      
         MVI   KEY,X'17'           URL by Rep record key                        
         MVC   K.RURLKREP,RCONKREP                                              
         DROP  K                                                                
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         JNE   EML025                                                           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    DMCB+8,X'FD'                                                     
         JZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVI   ELCODE,RURLSTX      Station tool kit URL element code            
         BRAS  RE,GETEL                                                         
         JNE   EML025                                                           
         USING RURLDETL,R6                                                      
         LLC   RE,RURLDETN                                                      
         SHI   RE,(RURLURL-RURLDETL)                                            
         STC   RE,SAVURLEN                                                      
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   SAVURL(0),RURLURL   Set to new URL                               
         EX    RE,0(RF)                                                         
*                                                                               
EML025   LA    R2,EMLRPLY          REPLY TO:                                    
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSALREC,KEY                                                      
         MVI   KEY,X'46'                                                        
         MVC   K.RSALKREP,RCONKREP                                              
         MVC   K.RSALKSAL,RCONSAL                                               
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DONOTRLY                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SAADDRX             GOTTA HAVE AN EML ADDRESS                    
*                                                                               
         USING RSASEMEM,R6                                                      
         CLI   RSASEMLN,L'RSASEMNM+2                                            
         BNH   SAADDRX                                                          
*                                                                               
         CLI   RSASEMLN,L'RSASEMNM+L'RSASEMFL+2                                 
         BNH   SAADDRX                                                          
         ZIC   RF,RSASEMLN                                                      
         SHI   RF,L'RSASEMNM+L'RSASEMFL+2+1                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RSASEAML                                                 
*                                                                               
         TM    RSASEMFL,X'80'     EMAIL SA ONLY, BYPASS S/P                     
         BO    SALADDX                                                          
*                                                                               
         AHI   RF,1                                                             
         AR    R2,RF                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
SAADDRX  DS    0H                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'        S/P EMAIL ADDRESS                            
         BRAS  RE,GETEL                                                         
         BNE   SALADDX                                                          
*                                                                               
         USING RSALEMEM,R6                                                      
         ZIC   R5,RSALEMLN                                                      
         SHI   R5,3                1 FOR EX, 1 LEN, 1 ELT CODE                  
         CHI   R5,0                                                             
         BL    SALADDX                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RSALEMAL                                                 
*                                                                               
SALADDX  DS    0H                                                               
         B     SALPRT                                                           
*                                                                               
DONOTRLY DS    0H                                                               
         MVC   0(L'NOREPLY,R2),NOREPLY    DONOTREPLY@MEDIAOCEAN.COM             
*                                                                               
SALPRT   DS    0H                  SET FOR FROM FIELD WORK                      
         OC    WORK2(L'SPACE150),SPACE150                                       
         MVC   WORK2(L'EMLRPLY),EMLRPLY                                         
*                                                                               
*                                  FROM:                                        
*                                  FROM FIELD ONLY TAKES 1 EMAIL ADD            
         LA    RE,WORK2                                                         
         LA    RF,WORK2+L'EMLRPLY                                               
EML23A   DS    0H                                                               
         CR    RE,RF                                                            
         BE    EML23C                                                           
         CLI   0(RE),C','          DELETE ALL AFTER 1ST COMMA                   
         BE    EML23B                                                           
         LA    RE,1(RE)                                                         
         B     EML23A                                                           
*                                                                               
EML23B   DS    0H                                                               
         XC    0(L'EMLRPLY,RE),0(RE)                                            
*                                                                               
EML23C   DS    0H               NOW TEDIOUSLY SET THE "FROM" EMLFMADR           
*                                                                               
         LA    R1,CONSALN          COUNT CHARACTERS IN CONSALN                  
         LA    R5,18(R1)                                                        
*                                                                               
EML23D   DS    0H                                                               
         CLI   0(R5),X'40'         ANYTHING ?                                   
         BH    EML23E              YES                                          
         SHI   R5,1                MOVE TO "LEFT" IN STRING                     
         CR    R5,R1                                                            
         BL    EML23F              NO CONSALN FOUND                             
         B     EML23D                                                           
*                                                                               
EML23E   DS    0H                                                               
         LA    R8,EMLFMADR                                                      
         MVI   0(R8),C'"'          SET QUOTES BEFORE NAME                       
         SR    R5,R1                                                            
         EX    R5,*+8              R5 ALREADY "1 LESS" (EML23D ABOVE            
         B     *+10                   DOES NOT COUNT FIRST CHARACTER)           
         MVC   1(0,R8),CONSALN                                                  
         LA    R8,2(R8,R5)         POINTING AFTER NAME                          
         MVI   0(R8),C'"'          SET QUOTES AFTER NAME                        
*                                                                               
EML23F   DS    0H                                                               
*                                                                               
         LA    R1,WORK2            COUNT CHARACTERS IN WORK2                    
         LA    R5,75(R1)                                                        
*                                                                               
EML23H   DS    0H                                                               
         CLI   0(R5),X'40'         ANYTHING ?                                   
         BH    EML23J              YES                                          
         SHI   R5,1                MOVE TO "LEFT" IN STRING                     
         CR    R5,R1                                                            
         BL    EML23L              NO WORK2 ENTRY FOUND                         
         B     EML23H                                                           
*                                                                               
EML23J   DS    0H                                                               
         LA    R8,1(R8)                                                         
         MVI   0(R8),C'<'          SET BEFORE EMAIL ADDRESS                     
         SR    R5,R1                                                            
         EX    R5,*+8              R5 ALREADY "1 LESS" (EML23H ABOVE            
         B     *+10                   DOES NOT COUNT FIRST CHARACTER)           
         MVC   1(0,R8),WORK2                                                    
         LA    R8,2(R8,R5)         POINTING AFTER NAME                          
         MVI   0(R8),C'>'          SET AFTER EMAIL ADDRESS                      
         B     EML23X                                                           
*                                                                               
EML23L   DS    0H                                                               
*                                                                               
         MVI   1(R8),C'<'                                                       
         MVC   2(L'NOREPLY,R8),NOREPLY    <DONOTREPLY@MEDIAOCEAN.COM>           
         MVI   2+L'NOREPLY(R8),C'>'                                             
*                                                                               
EML23X   DS    0H                  *****  SUBJECT LINE  *****                   
*                                                                               
         LA    R3,EMLSUBJ          POINT TO SUBJECT LINE                        
         USING EDIREML,R3                                                       
*                                                                               
         XC    EDIREML(70),EDIREML                                              
*                                                                               
         MVC   EDIRSTX,=C'*STK*'                                                
         MVC   EDIRSTA(4),RCONKSTA                                              
         CLI   RCONKSTA+4,C' '                                                  
         BNH   EML32                                                            
*                                                                               
         LA    RE,EDIRSTA+3        CHOP SPACES                                  
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),RCONKSTA+4                                               
*                                                                               
EML32    DS    0H                  CONTRACT #                                   
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,EDIRCON),ALIGN=LEFT                                 
*                                                                               
         MVC   DUB(4),ACOMFACS     FLIGHT START, END                            
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         GOTO1 DATCON,DMCB,(0,WORK),(5,EDIRSTD)                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(5,EDIREND)                               
         MVI   DASH,C'-'                                                        
         MVC   EDIRADV(L'CONADVN),CONADVN                                       
*                                                                               
         LA    R2,EDIRADV+L'CONADVN-1                                           
         CLI   0(R2),X'40'         CHOP SPACES                                  
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVC   EDIROFF,RCONKOFF                                                 
*                                                                               
         CLC   RCONTYPE,XTRACTY                                                 
         BE    EML38                                                            
         CLI   RCONTYPE,C'N'                                                    
         BE    EML38                                                            
         CLI   RCONTYPE,C'X'                                                    
         BE    EML38                                                            
         CLC   MASTEREP,=C'K3'                                                  
         BNE   EML39                                                            
         CLI   RCONTYPE,C'D'                                                    
         BE    EML38                                                            
*                                                                               
         B     EML39                                                            
*                                                                               
EML38    DS    0H                                                               
         MVC   EDIRNET,=C'UNWIRED'                                              
*                                                                               
EML39    DS    0H                                                               
         OC    EDIREML(70),SPACE150                                             
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
         DROP  R3                                                               
*                                  *********************                        
*                                  BODY OF EMAIL FOLLOWS                        
*                                  *********************                        
*                                                                               
         LA    R3,EMLDATA          START OF THE VARIABLE STRING                 
         LA    R0,40                                                            
BLKLUP   MVC   0(100,R3),SPACE150                                               
         LA    R3,100(R3)          SET 4000 BYTE STRING AREA TO SPACES          
         BCT   R0,BLKLUP                                                        
*                                                                               
         LA    R3,EMLDATA          START OF THE VARIABLE STRING                 
         ST    R3,LNTHADR          2-BYTE LENGTH GOES TO THIS ADDRESS           
         LA    R8,0                ACCUMULATE LENGTH IN R8                      
*                                                                               
         LA    R6,RCONREC          CONTRACT STATUS + CONTRACT NUM               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EML46                                                            
*                                                                               
         USING RCONSNCO,R6                                                      
*                                                                               
         LA    R3,2(R3)            BUMP PAST LENGTH BYTES TO DATA AREA          
*                                                                               
         ZIC   R5,CONCNUMH+5                                                    
         BCTR  R5,0                                                             
         MVC   0(10,R3),=CL10'NEW ORDER#'                                       
*                                                                               
         CLI   RCONSRV,1                                                        
         BNE   EML040                                                           
         LA    R8,10(R8)           BUMP LENGTH COUNTER                          
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   11(0,R3),CONCNUM                                                 
         LA    R8,2(R5,R8)         BUMP LENGTH COUNTER                          
         B     EML042                                                           
*                                                                               
EML040   DS    0H                                                               
         LA    R8,14(R8)           BUMP LENGTH COUNTER                          
         MVC   0(14,R3),=CL14'REVISED ORDER#'                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R3),CONCNUM                                                 
         LA    R8,2(R5,R8)         BUMP LENGTH COUNTER                          
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
EML042   DS    0H                                                               
         L     R3,LNTHADR          POINT TO 2-BYTE LINE LENGTH                  
         STCM  R8,3,0(R3)          SET LENGTH FOR ABOVE LINE                    
         LA    R3,2(R8,R3)         START OF NEXT LINE LENGTH FIELD              
*                                                                               
*                                  **** SKIP A LINE ****                        
         MVC   0(2,R3),=X'0002'    LENGTH OF LINE                               
         MVC   2(2,R3),SPACE150    A 2-CH BLANK LINE                            
*                                                                               
         LA    R3,4(R3)            START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
         LA    R3,2(R3)            START OF NEXT LINE DATA                      
         LA    R8,0                CLEAR LENGTH COUNTER                         
*                                                                               
         USING EDICNTD,R3          BODY, CONTRACT DETAIL                        
         MVC   EDIAGY(L'CONAGYN),CONAGYN                                        
         MVC   EDISAL(L'CONSALN),CONSALN                                        
*                                                                               
         LA    R6,RCONREC          CONTRACT TOTAL                               
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   EML44                                                            
*                                                                               
         USING RCONXEL,R6                                                       
         EDIT  RCONTOT,EDITOT,2,ZERO=NOBLANK,FLOAT=$                            
*                                                                               
EML44    LA    R6,RCONREC          CONTRACT STATUS                              
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EML45X                                                           
         USING RCONSNCO,R6                                                      
*                                                                               
         SR    R1,R1                                                            
         MVC   EDISTAT(3),=C'New'                                               
         LA    R1,3                                                             
         CLI   RCONSRV,1                                                        
         BE    EML45X                                                           
*                                                                               
EML45    DS    0H                                                               
         MVC   EDISTAT(8),=C'Revised#'                                          
         LA    R1,8                                                             
*                                                                               
         LA    RE,EDISTAT                                                       
         AR    RE,R1                                                            
         EDIT  RCONSRV,(3,(RE)),ALIGN=LEFT                                      
*                                                                               
EML45X   DS    0H                                                               
         LA    R8,EDICNQ(R8)     BUMP THE LENGTH                                
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
EML46    DS    0H                                                               
         L     R3,LNTHADR          POINT TO NEXT LINE LENGTH                    
         LTR   R8,R8               ANY LINE LENGTH ?                            
         BZ    EML47               ZERO MEANS NO LINE CREATED                   
*                                                                               
         STCM  R8,3,0(R3)          SET LENGTH FOR ABOVE LINE                    
         LA    R3,2(R8,R3)         START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
*                                                                               
EML47    DS    0H                  **** SKIP A LINE ****                        
         MVC   0(2,R3),=X'0002'    LENGTH OF BLANK LINE                         
         MVC   2(2,R3),SPACE150    A 2-CH BLANK LINE                            
*                                                                               
         LA    R3,4(R3)            START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
         LA    R3,2(R3)            START OF NEXT LINE DATA                      
         LA    R8,0                CLEAR LENGTH COUNTER                         
*                                                                               
* REP ORDER COMMENTS                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'        REP ORDER COMMENT                            
         BAS   RE,GETEL                                                         
         BNE   EMLOCMX                                                          
*                                                                               
EMLOCM10 DS    0H                                                               
         CLC   =C'SC=',2(R6)                                                    
         BE    EMLOCM15                                                         
         CLC   =C'C=',2(R6)                                                     
         BNE   EMLOCM30                                                         
*                                                                               
EMLOCM15 DS    0H                  FAKE A FIELD HEADER                          
         XC    WORK2,WORK2                                                      
         ZIC   RE,1(R6)                                                         
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+8(0),2(R6)                                                 
*                                                                               
         AHI   RE,1                                                             
         STC   RE,WORK2+5                                                       
         MVI   WORK2,100                                                        
*                                                                               
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(1,WORK2),(R2),DATAMGR,RCONREC,GETTXT              
         BNZ   EMLOCM30            COMMENT NOT FOUND                            
*                                                                               
         OC    WORK2+8(60),SPACE150                                             
         MVC   0(60,R3),WORK2+8                                                 
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
         LA    RE,60               LENGTH OF COMMENT                            
         L     R3,LNTHADR          POINT TO NEXT LINE LENGTH                    
         STCM  RE,3,0(R3)          SET LENGTH FOR ABOVE LINE                    
         LA    R3,2(RE,R3)         START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
         LA    R3,2(R3)            START OF NEXT LINE DATA                      
         B     EMLOCM50                                                         
*                                                                               
EMLOCM30 DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         AHI   R2,-3                                                            
*                                                                               
         LTR   R2,R2                                                            
         BM    EMLOCM50                                                         
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R6)                                                    
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),SPACE150                                                 
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
         AHI   R2,1                ACTUAL LENGTH OF COMMENT                     
         L     R3,LNTHADR          POINT TO NEXT LINE LENGTH                    
         STCM  R2,3,0(R3)          SET LENGTH FOR ABOVE LINE                    
         LA    R3,2(R2,R3)         START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
         LA    R3,2(R3)            START OF NEXT LINE DATA                      
*                                                                               
EMLOCM50 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    EMLOCM10                                                         
*                                                                               
EMLOCMX  DS    0H                  **** SKIP A LINE ****                        
         L     R3,LNTHADR          POINT TO NEXT LINE LENGTH                    
         MVC   0(2,R3),=X'0002'    LENGTH OF BLANK LINE                         
         MVC   2(2,R3),SPACE150    A 2-CH BLANK LINE                            
*                                                                               
         LA    R3,4(R3)            START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
*                                                                               
* URL                                                                           
*                                                                               
         LA    RF,MSG1Q            MESSAGE 1 LENGTH                             
         STCM  RF,3,0(R3)          GOES TO LINE LENGTH                          
         MVC   2(MSG1Q,R3),MSG1    MESSAGE TO LINE                              
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
         LA    R3,2(R3,RF)         START OF NEXT LINE LENGTH                    
         LA    RF,MSG2Q            MESSAGE 2 LENGTH                             
         STCM  RF,3,0(R3)          GOES TO LINE LENGTH                          
         MVC   2(MSG2Q,R3),MSG2    MESSAGE TO LINE                              
*                                                                               
******   END OF "LINE"                                                          
*                                                                               
         LA    R3,2(R3,RF)         START OF NEXT LINE LENGTH                    
         ST    R3,LNTHADR          SAVE IT                                      
         LA    R3,2(R3)            START OF NEXT LINE DATA                      
*                                                                               
*        LIVE TAG (SOM.MEDIAOCEAN.COM)                                          
*                                                                               
         XC    0(080,R3),0(R3)                                                  
         LA    R3,EDIURL            HTTPS://                                    
         CLI   URL3DOT0,C'S'       STX 1.X USER?                                
         BE    URLA0020            YES - OUTPUT 1.X URL                         
         CLI   URL3DOT0,C'Y'       3.0 USER?                                    
         BE    URLA0080            YES - OUTPUT 3.0 URL                         
*                                  NO  - OUTPUT 2.1 URL                         
         MVC   0(L'HTTPST21,R3),HTTPST21                                        
         LA    R3,L'HTTPST21(R3)                                                
         B     URLA0100                                                         
URLA0020 EQU   *                                                                
         OC    SAVURL,SAVURL       URL FROM REP RECORD?                         
         BZ    URLA0030            NO  - TAKE FROM HARD-CODED LIST              
         ZIC   RF,SAVURLEN         YES - SET ITS LENGTH                         
         BCTR  RF,0                REDUCE FOR EX                                
         EX    RF,URLA0021         MOVE DATA BY LENGTH                          
         B     URLA0022                                                         
URLA0021 MVC   0(0,R3),SAVURL      INSERT URL FROM REP RECORD                   
URLA0022 EQU   *                                                                
         LA    RF,1(RF)            RESET TOTAL LENGTH                           
         AR    R3,RF               SET END OF DATA                              
*                                                                               
*   PER HARLAN BARNES:  ADD A '?' AT END OF STRING                              
*                                                                               
         MVI   0(R3),C'?'                                                       
         LA    R3,1(R3)            BUMP PAST THE "?"                            
*                                                                               
         MVC   0(15,R3),=C'contractNumber='                                     
         LA    R3,15(R3)           SET A(END OF STRING)                         
         B     URLA0035                                                         
URLA0030 EQU   *                                                                
         MVC   0(L'HTTPSTX1,R3),HTTPSTX1                                        
         LA    R3,L'HTTPSTX1(R3)                                                
*                                                                               
*   STX URL REQUIRES CONTRACT NUMBER AND STATION CALL LETTERS                   
*                                                                               
URLA0035 EQU   *                                                                
         ZIC   R5,CONCNUMH+5        APPEND CON# TO THE END                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     URLA0040                                                         
         MVC   0(0,R3),CONCNUM                                                  
URLA0040 EQU   *                                                                
         LA    R5,1(R5)            RESET FULL LENGTH                            
         AR    R3,R5               SET A(NEW END OF STRING)                     
         MVI   0(R3),AMPER         INSERT AN & INTO LINE                        
         MVC   1(12,R3),=C'stationCall='                                        
         LA    R3,13(R3)           SET A(NEW END OF STRING)                     
         MVC   0(5,R3),RCONKSTA    INSERT STATION CALL LETTERS                  
         LA    R3,4(R3)            CHECK LAST POSITION FOR SPACE                
         CLI   0(R3),C' '          MEDIA = SPACE?                               
         BNH   URLA0060            YES                                          
         LA    R3,1(R3)            NO  - BUMP PAST MEDIA VALUE                  
URLA0060 EQU   *                                                                
         MVI   0(R3),AMPER         INSERT AN & INTO LINE                        
         MVC   1(08,R3),=C'alphaId='                                            
         LA    R3,09(R3)           SET A(NEW END OF STRING)                     
         MVC   0(2,R3),RCONKREP    INSERT REP POWER CODE                        
*                                                                               
         B     URLA0120                                                         
URLA0080 EQU   *                                                                
         MVC   0(L'HTTPST30,R3),HTTPST30                                        
         LA    R3,L'HTTPST30(R3)                                                
         B     URLA0100                                                         
URLA0100 EQU   *                                                                
*                                                                               
*        TEST TAG (POSEIDON.MEDIAOCEAN.COM)                                     
*        XC    0(80,R3),0(R3)                                                   
*        LA    R3,EDIURL            HTTPS://                                    
*        MVC   0(L'HTTPSTST,R3),HTTPSTST                                        
*        LA    R3,L'HTTPSTST(R3)                                                
*                                                                               
*        TEST TAG (NEMO.MEDIAOCEAN.COM)                                         
*        XC    0(80,R3),0(R3)                                                   
*        LA    R3,EDIURL            HTTPS://                                    
*        MVC   0(L'HTTPSNEM,R3),HTTPSNEM                                        
*        LA    R3,L'HTTPSNEM(R3)                                                
*                                                                               
         ZIC   R5,CONCNUMH+5        APPEND CON# TO THE END                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CONCNUM                                                  
*                                                                               
URLA0120 EQU   *                                                                
*                                                                               
******   END OF "LINE"                                                          
*                              **  ADD UP AND SET LENGTH OF URL LINE **         
         L     R3,LNTHADR          POINT TO BEGINNING OF URL LINE               
         LA    R3,2(R3)            POINT TO BEGINNING OF URL DATA               
         LA    RF,0                COUNTER                                      
*                                                                               
URLLUP   EQU   *                                                                
         CLI   0(R3),C' '          DATA THERE ?                                 
         BNH   URLEND              NO - DONE WITH COUNT                         
         LA    RF,1(RF)            ADD TO COUNTER                               
         LA    R3,1(R3)            BUMP RIGHT IN URL                            
         B     URLLUP              NEXT                                         
*                                                                               
URLEND   EQU   *                                                                
         MVC   0(2,R3),=X'FFFF'    END OF VARIABLE EMAIL DATA INDICATOR         
         L     R3,LNTHADR          POINT TO BEGINNING OF URL LINE               
         STCM  RF,3,0(R3)          SET LENGTH OF URL THERE                      
         CHI   RF,160                                                           
         BNH   *+6                 160 IS MAX LENGTH FOR URL                    
         DC    H'0'                                                             
*                                                                               
SENDEML  EQU   *                                                                
*                                                                               
*        FINAL SETTINGS FOR JESMAIL AND EXIT (SEE FAJESMAILD)                   
*                                                                               
         LA    R1,SMTPC            ESTABLISH EMAIL PARAMETER LIST               
         USING SMTPD,R1                                                         
         XC    SMTPC(36),SMTPC     INIT PARAMETER BLOCK                         
*                                                                               
         LA    RF,EMLTOADR                                                      
         ST    RF,SMTPTO           SET TO ADDRESS                               
         LA    RF,EMLCCADR                                                      
         CLI   EMLCCADR,C' '       ANYTHING IN CC ADDRESS                       
         BNH   *+8                 NO - LEAVE SMTPCC NULLS                      
         ST    RF,SMTPCC           SET CC ADDRESS                               
         LA    RF,EMLBCADR                                                      
         ST    RF,SMTPBCC          SET BCC ADDRESS                              
         LA    RF,EMLFMADR                                                      
         ST    RF,SMTPFROM         SET FROM ADDRESS                             
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          SET SUBJECT ADDRESS                          
         LA    RF,EMLDATA                                                       
         ST    RF,SMTPDATA         SET BODY OF EMAIL ADDRESS                    
         LA    RF,SMTPMAIL                                                      
         ST    RF,SMTPVRLN         INDICATE DATA IS VARIABLE                    
*                                                                               
         LA    RF,STXSNDR          SET SENDER/FROM WHO EMAIL ADDRESS            
         ST    RF,SMTPSNDR                                                      
*                                                                               
         MVC   EMLJMOPT,JMOPTION   'LONGER FROM'/'SENDER'/'REPLYTO'             
         LA    RF,EMLJMOPT                                                      
         ST    RF,SMTPOPTS                                                      
*                                                                               
         CLI   EMLRPLY,C' '        ANYTHING ?                                   
         BH    SENDEMLF            YES - SET REPLY                              
         MVI   EMLJMOPT+L'FROMLONG+L'SENDER,X'FF'    NO REPLY FIELD             
         B     SENDEMLX            DONE                                         
*                                                                               
SENDEMLF EQU   *                                                                
         LA    RF,EMLRPLY          SET REPLY ADDRESS                            
         ST    RF,SMTPRPLY                                                      
*                                                                               
SENDEMLX EQU   *                                                                
         GOTOR AJESMAIL,(R1)       SEND E-MAIL                                  
*                                                                               
         DROP  R1,R3,R4,R6,R7                                                   
*                                                                               
NOEMLER  EQU   *                   NO EMAIL ADDRESS FOUND                       
*                                                                               
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
HTTPST21 DC    CL68'https://som.mediaocean.com/webconfirm/ctrl/wcfdetaiX        
               l?contractNumber='                                               
*                                                                               
HTTPST30 DC    CL69'https://som3.mediaocean.com/webconfirm/ctrl/wcfdetaX        
               il?contractNumber='                                              
*&&DO                                                                           
*   per harlan barnes:  backup url changed to 'redirect'                        
*                                                                               
HTTPSTX1 DC    CL73'https://som3.mediaocean.com/stationxpress/ctrl/openX        
               detail?contractNumber='                                          
*&&                                                                             
HTTPSTX1 DC    CL77'https://redirect.mediaocean.com/stationxpress/ctrl/X        
               opendetail?contractNumber='                                      
*                                                                               
HTTPSTST DC    CL73'https://poseidon.mediaocean.com/webconfirm/ctrl/wcfX        
               detail?contractNumber='                                          
*                                                                               
HTTPSNEM DC    CL69'https://nemo.mediaocean.com/webconfirm/ctrl/wcfdetaX        
               il?contractNumber='                                              
*                                                                               
BCCADR   DC    CL32'StationXpressElog@mediaocean.com'                           
*                                                                               
NOREPLY  DC    C'DONOTREPLY@MEDIAOCEAN.COM'                                     
*                                                                               
STXSNDR  DC    CL150'<StationXpressSender@mediaocean.com>'                      
*                                                                               
MSG1     DC    C'To view the details of this order, click the '                 
         DC    C'following link'                                                
MSG1Q    EQU   *-MSG1                                                           
MSG2     DC    C'(for security purposes, you will be prompted to '              
         DC    C'log in):'                                                      
MSG2Q    EQU   *-MSG2                                                           
*                                                                               
AMPER    EQU   X'50'               AMPERSAND                                    
SPACE150 DC    150C' '                                                          
*                              DO NOT CHANGE SEQUENCE OF BELOW 4 FIELDS         
SMTPMAIL DC    CL10'*SMTPMAIL*'                                                 
JMOPTION DS    0H                                                               
FROMLONG DC    CL10'*FROMLONG*'                                                 
SENDER   DC    CL10'*SENDER***'                                                 
REPLYTO  DC    CL10'*REPLYTO**'                                                 
         DC    X'FF'                                                            
JMOPTNQ  EQU   *-JMOPTION                                                       
*                              DO NOT CHANGE SEQUENCE OF ABOVE 4 FIELDS         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         ORG     EDICOMN                                                        
EDIREML  DS   0CL80                                                             
EDIRSTX  DS    CL5                                                              
EDIRSTA  DS    CL6                                                              
         DS    CL1                                                              
*EDIRCONL DS    CL7                                                             
*         DS    CL1                                                             
EDIRCON  DS    CL8                                                              
         DS    CL1                                                              
EDIRSTD  DS    CL8         FLIGH START DATE                                     
DASH     DS    CL1         -                                                    
EDIREND  DS    CL8         FLIGH END DATE                                       
         DS    CL1                                                              
EDIRADV  DS    CL20        ADVERTISER NAME                                      
         DS    CL1                                                              
EDIROFF  DS    CL2         SALESPERSON OFFICE                                   
         DS    CL1                                                              
EDIRNET  DS    CL7         UNWIRED/NETWORK LABEL                                
         DS    (L'EDIREML-(*-EDIREML))X                                         
*                                                                               
EDICNTD  DSECT                                                                  
EDIAGY   DS    CL20        AGENCY NAME                                          
         DS    CL3                                                              
EDISAL   DS    CL20        SALESPERSON NAME                                     
         DS    CL3                                                              
EDITOT   DS    CL12        TOTAL COST                                           
         DS    CL3                                                              
EDISTAT  DS    CL11        STATUS                                               
EDICNQ   EQU   *-EDIAGY                                                         
         ORG   EDIAGY                                                           
EDIURL   DS    CL80                                                             
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
MYWRKD   DSECT                                                                  
RELO     DS    F                                                                
MASTEREP DS    CL2                                                              
XTRACTY  DS    CL1                                                              
SAVURL   DS    CL72                                                             
SAVURLEN DS    XL1                                                              
BIGSPLKY DS    XL128                                                            
SAVEKEY  DS    XL48                                                             
WORK2X   DS    XL240                                                            
FORMAT   DS    CL1                                                              
URL3DOT0 DS    CL1                                                              
SVRSTAP6 DS    CL(L'RSTAP6)        Station profile 6                            
LNTHADR  DS    F                                                                
SAVER3   DS    F                                                                
AJESMAIL DS    V                                                                
*                                                                               
*        JESMAIL PARAMETER BLOCK                                                
*                                                                               
         DS    0F                                                               
SMTPC    DS    XL(SMTPDQ)          PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
*        E-MAIL FIELDS                                                          
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL60                TO:E-MAIL ADDRESS                            
EMLTOEND DS    XL1                 X'FF' END OF LIST                            
EMLCCADR DS    CL180               CC: E-MAIL ADDRESS (UP TO THREE)             
EMLCCEND DS    XL1                 X'FF' END OF LIST                            
EMLBCADR DS    CL60                BCC: E-MAIL ADDRESS                          
EMLBCEND DS    XL1                 X'FF' END OF LIST                            
EMLFMADR DS    CL150               FROM: E-MAIL ADDRESS                         
EMLRPLY  DS    CL150               UP TO 2 REPLY TO EMAIL ADDRESSES             
EMLSUBJ  DS    CL70                SUBJECT                                      
EMLJMOPT DS    CL(JMOPTNQ)         EMAIL OPTIONS                                
EMLDATA  DS    CL4000              VARIABLE STRING - SEE FAJESMAILD             
EMLFLDLN EQU   *-EMLFLDS                                                        
*                                                                               
MYWRKDX  EQU     *                                                              
GENSAL2  DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE FAJESMAILD                                                     
GENURL   DSECT                                                                  
       ++INCLUDE REGENURL                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140RECNT7A   01/09/13'                                      
         END                                                                    
