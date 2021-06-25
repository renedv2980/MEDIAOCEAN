*          DATA SET ACLFM22    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T60322A                                                                  
         TITLE 'MODULE TO HANDLE APG RULES'                                     
T60322   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM22*,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
*                                                                               
         USING COMFACSD,R6                                                      
         L     R6,COMFACS                                                       
         MVC   VGETTXT,CGETTXT                                                  
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 1                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   AC10                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'F'          ALWAYS UNIT F                                
         SPACE 1                                                                
         LA    R2,LOGLEDGH                                                      
         GOTO1 ANY                                                              
         MVC   KEY+2(1),LOGLEDG                                                 
         TM    4(R2),X'20'                                                      
         BO    AC4                                                              
         FOUT  LOGLNAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,YES                                                       
         SPACE 1                                                                
         MVI   ELCODE,X'16'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEHEIR,0(R4)                                                   
         SPACE 1                                                                
AC4      LA    R2,LOGACCH                                                       
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK                                                   
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,YES                                                       
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              DISPLAY ACCOUNT DETAILS                                          
         SPACE 1                                                                
AC10     CLI   MODE,DSPLYREC                                                    
         BNE   ACB20                                                            
         LA    R2,LOGNMDSH                                                      
         GOTO1 NAMOUT                                                           
         BAS   RE,ACDSLY           DISPLAY  ELEMENTS                            
         LA    R2,LOGANAMH                                                      
         B     XIT                                                              
         EJECT                                                                  
ACDSLY   NTR1                                                                   
         MVI   ERRFLG,NO           INIT ERRFLG TO N                             
         TWAXC LOGSECH                                                          
         GOTO1 STATIN                                                           
         LA    R4,ELEMENT                                                       
         USING ACSTATD,R4                                                       
         CLC   ACSTSECY,SPACES                                                  
         BE    ACD10                                                            
         EDIT  (2,ACSTSECY),(3,LOGSEC),ALIGN=LEFT                               
         SPACE 1                                                                
         USING FIND,R6                                                          
ACD10    LA    R6,LOGACCSH                                                      
         LA    R4,IO2                                                           
*                                                                               
*  CHECK TO SEE IF RECORD IS TOO BIG FOR =FILE                                  
*                                                                               
         ST    R4,SVREG            SAVE R4                                      
         MVI   ELCODE,GLPELQ       X'15' ELEMENT                                
         SR    R3,R3                                                            
         BAS   RE,GETEL                                                         
         B     ACD12                                                            
ACD11    BAS   RE,NEXTEL                                                        
ACD12    BNE   ACD13                                                            
         AHI   R3,1                                                             
         CHI   R3,10               CAN ONLY HANDLE 10 ELEMENTS                  
         BNH   ACD11                                                            
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
         LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,=AL2(AE$UNFIL)  RECORD TOO BIG FOR =FILE                 
         MVI   ERRFLG,YES                                                       
*                                                                               
         GOTO1 VGETTXT,DMCB                                                     
         DROP  R1                                                               
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
ACD13    L     R4,SVREG            RESTORE R4                                   
*                                                                               
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
         USING ACGENLD,R4                                                       
ACD14    MVC   ACCWK,SPACES                                                     
         MVC   FINFLT,ACGLFLT                                                   
         MVC   FINTYP,ACGLBTYP                                                  
         MVC   FINACT,ACGLACT                                                   
         MVC   ACCWK(14),ACGLACC                                                
         CLI   ACGLLEN,X'1A'                                                    
         BE    UNSPACC                                                          
         MVC   ACCWK+14(14),ACGLACC2                                            
UNSPACC  LA    R7,ACCWK                                                         
         LA    R3,FINACC                                                        
         BAS   RE,APGUNSPL                                                      
         MVC   ACCWK,SPACES                                                     
         CLI   ACGLLEN,X'28'                                                    
         BNH   ACD15                                                            
         MVC   ACCWK(14),ACGLCON                                                
         CLI   ACGLLEN,X'36'                                                    
         BE    UNSPCON                                                          
         MVC   ACCWK+14(14),ACGLCON2                                            
UNSPCON  LA    R7,ACCWK                                                         
         LA    R3,FINCON                                                        
         BAS   RE,APGUNSPL                                                      
         SPACE 1                                                                
ACD15    LA    R6,FINLEN(R6)                                                    
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         B     ACD14                                                            
         EJECT                                                                  
*              BUILD AN ACCOUNT RECORD                                          
         SPACE 1                                                                
ACB20    LA    R2,LOGANAMH                                                      
         CLI   ERRFLG,YES                                                       
         BNE   ACB20A                                                           
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
         LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,=AL2(AE$UNFIL)  RECORD TOO BIG FOR =FILE                 
         GOTO1 VGETTXT,DMCB                                                     
         DROP  R1                                                               
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
*                                                                               
ACB20A   DS    0H                                                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         GOTO1 NAMIN                                                            
         CLC   WORK(7),=C'DELETE '                                              
         BE    AC30                                                             
         CLI   LOGACT,NO                                                        
         BNE   ACB21                                                            
         GOTO1 STATIN                                                           
ACB21    LA    R4,IO2                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R4                                                       
         XC    ACSTSECY,ACSTSECY                                                
         LA    R2,LOGSECH                                                       
         CLI   5(R2),0                                                          
         BE    ACB22                                                            
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         STH   R1,ACSTSECY                                                      
         CH    R1,=H'256'                                                       
         BL    ACB22                                                            
         MVI   ERROR,ONETO255                                                   
         B     XIT                                                              
         SPACE 2                                                                
ACB22    GOTO1 REMANEL,DMCB,(X'15',0)                                           
         LA    R6,LOGACCSH                                                      
         USING FIND,R6                                                          
         USING ACGENLD,R4                                                       
ACB24    LA    R4,ELEMENT                                                       
         MVC   ELEMENT,SPACES                                                   
         MVC   ACGLEL(2),=X'151A'                                               
         CLI   FINACCH+5,0                                                      
         BE    ACB30               NO MORE INPUT                                
         SPACE 1                                                                
         LA    R3,FINACC           CHECK THE ACCOUNT FIELD(S)                   
         LA    R7,ACGLACC                                                       
         MVI   SPLCA,C'A'          INDICATE ACCOUNT(S) BEING PASSED             
         BAS   RE,APGSPLIT                                                      
         OC    ACGLACC,SPACES                                                   
         OC    ACGLACC2,SPACES                                                  
         LA    R2,FINACCH                                                       
         CLI   SPLERR,C'1'         TEST ERR IND PASSED FROM APGSPLIT            
         BE    INVINPUT                                                         
         CLI   SPLERR,C'2'                                                      
         BE    INVLEDG                                                          
         CLI   SPLERR,C'3'         ACCOUNT SPLIT TWICE                          
         BE    INVSPLIT                                                         
         CLI   SPLERR,C'4'         SPLIT AND HAS WILD CARDS                     
         BE    INVWILD                                                          
         CLI   ACGLACC,X'40'       MUST HAVE AT LEAST 1 ACCOUNT NUMBER          
         BE    INVINPUT                                                         
         SPACE 1                                                                
         LA    R3,FINCON           CHECK THE CONTRA FIELD(S)                    
         LA    R7,ACGLCON                                                       
         MVI   SPLCA,C'C'          INDICATE CONTRA(S) BEING PASSED              
         BAS   RE,APGSPLIT                                                      
         OC    ACGLCON,SPACES                                                   
         OC    ACGLCON2,SPACES                                                  
         LA    R2,FINCONH                                                       
         CLI   SPLERR,C'1'                                                      
         BE    INVINPUT                                                         
         CLI   SPLERR,C'3'         ACCOUNT SPLIT TWICE                          
         BE    INVSPLIT                                                         
         CLI   SPLERR,C'4'         SPLIT AND HAS WILD CARDS                     
         BE    INVWILD                                                          
         CLI   ACGLCON,X'40'       IF THERE IS NO FROM CONTRA                   
         BNE   *+12                                                             
         CLI   ACGLCON2,X'40'      THERE CAN'T BE A TO CONTRA.                  
         BNE   INVINPUT                                                         
         SPACE 1                                                                
*                                  ALL GOOD SO DEVELOPE PROPER ELEMENT          
         MVC   ACGLEL(2),=X'151A'  MINIMUM LENGTH IS 26                         
         CLI   ACGLACC2,X'40'      ANY TO ACC                                   
         BE    *+8                                                              
         MVI   ACGLLEN,X'28'       40 CHAR LENGTH                               
         CLI   ACGLCON,X'40'       ANY FROM CONTRA                              
         BE    ACB25               NO, FINAL LENGTH IS 40                       
         MVI   ACGLLEN,X'36'       SET LENGTH TO 54                             
         CLI   ACGLCON2,X'40'      ANY TO CONTRA                                
         BE    ACB25                                                            
         MVI   ACGLLEN,X'44'       SET LENGTH TO 68 (MAXIMUM)                   
         SPACE 1                                                                
ACB25    MVC   ACGLFLT,FINFLT      FILTER FIELD                                 
         OC    ACGLFLT,SPACES                                                   
         SPACE 1                                                                
         MVC   ACGLBTYP,FINTYP                                                  
         OC    ACGLBTYP,SPACES                                                  
         SPACE 1                                                                
         MVI   ACGLACT,X'00'        ACTION                                      
         LA    R2,FINACTH                                                       
         CLI   FINACTH+5,1                                                      
         BH    INVINPUT                                                         
         CLI   FINACTH+5,0                                                      
         BE    ACB27                                                            
         MVC   ACGLACT(1),FINACT                                                
         CLI   ACGLACT,C'-'                                                     
         BE    ACB27               MINUS OK                                     
         CLI   ACGLACC2,C' '       ARE THERE 2 ACCOUNTS                         
         BE    INVINPUT                                                         
         CLI   ACGLACT,C'/'        IS IT DIVISION?                              
         BE    ACB27                                                            
         CLI   ACGLACT,C'%'        IS IT A PERCENT?                             
         BNE   INVINPUT            NO SO NOT VAILD SIGN                         
         SPACE 1                                                                
ACB27    GOTO1 ADDANEL             ADD THE ELEMENT                              
         LA    R6,FINLEN(R6)       NEXT LINE ON SCREEN                          
         CLI   0(R6),9                                                          
         BNE   ACB24               END OF SCREEN                                
         SPACE 1                                                                
ACB30    BAS   RE,ACDSLY           DISPLAY                                      
         EJECT                                                                  
*              SPECIAL ROUTINES FOR HEIRARCHY                                   
         SPACE 2                                                                
AC23     MVI   ERROR,X'FF'                                                      
         CLI   ACTION,NO                                                        
         BNE   XIT                                                              
         LA    R2,LOGACCH                                                       
         LA    R4,SAVEHEIR                                                      
         USING ACHEIRD,R4                                                       
         SR    R5,R5                                                            
         IC    R5,ACHRLEVA                                                      
         CLC   5(1,R2),ACHRLEVA                                                 
         BH    AC24                                                             
         CLI   ACHRLEVB,0                                                       
         BNE   XIT                                                              
         GOTO1 BALIN                                                            
         B     XIT                                                              
         SPACE 2                                                                
AC24     CLI   ACHRLEVB,0                                                       
         BE    TOOLON                                                           
         CLC   5(1,R2),ACHRLEVB                                                 
         BH    AC26                                                             
         CLI   ACHRLEVC,0                                                       
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
         SPACE 2                                                                
AC26     IC    R5,ACHRLEVB                                                      
         CLI   ACHRLEVC,0                                                       
         BE    TOOLON                                                           
         CLC   5(1,R2),ACHRLEVC                                                 
         BH    AC28                                                             
         CLI   ACHRLEVD,0                                                       
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
         SPACE 2                                                                
AC28     IC    R5,ACHRLEVC                                                      
         CLI   ACHRLEVD,0                                                       
         BE    TOOLON                                                           
         GOTO1 BALIN                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              SPECIAL DELETE CODE                                              
         SPACE 3                                                                
AC30     MVI   ERROR,67                                                         
         CLI   LOGACT,C'A'                                                      
         BNE   XIT                                                              
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*&&US                                                                           
         SPACE 2                                                                
AC31     CLI   0(R4),0                                                          
         BE    AC31X                                                            
         CLI   0(R4),X'62'         CAN'T DELETE RECORDS WITH A                  
         BNE   AC31B               SCHEME CODE                                  
         USING ACDISTD,R4                                                       
         CP    ACDIVAL,=P'0'                                                    
         BNE   XIT                 ERROR CAN'T DELETE                           
AC31B    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC31                                                             
AC31X    LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*&&                                                                             
AC32     CLI   0(R4),0                                                          
         BE    AC36                                                             
         CLI   0(R4),X'30'                                                      
         BNE   AC33                                                             
         USING ACSTATD,R4                                                       
         OC    ACSTBFDT,ACSTBFDT   OLD RECORDS                                  
         BZ    AC33                                                             
*&&US                                                                           
         LA    RF,IO2                                                           
         CLI   1(RF),C'3'          SKIP TEST BELOW IF RETAIL                    
         BE    AC33                                                             
*&&                                                                             
         CLC   ACSTLAST,ACSTBFDT   THIS CHECKS FOR SELF-BALANCING DR/CR         
         BH    XIT                                                              
         SPACE 2                                                                
AC33     CLI   0(R4),X'32'                                                      
         BNE   AC34                                                             
         USING ACBALD,R4                                                        
         CP    ACBLFRWD,=P'0'      ACCOUNT MUST HAVE BALANCE ELEMENT            
         BNE   XIT                 AND ALL BALANCES MUST BE ZERO                
         CP    ACBLDR,=P'0'                                                     
         BNE   XIT                                                              
         CP    ACBLCR,=P'0'                                                     
         BNE   XIT                                                              
         LA    RF,IO2                                                           
         OI    44(RF),X'80'                                                     
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         SPACE 2                                                                
AC34     SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     AC32                                                             
         SPACE 2                                                                
AC36     MVC   KEY,IO2             IF RECORD HAS NO BALANCE ELEMENT,            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 READ                WE NEED TO READ THE NEXT RECORD TO           
         OI    DMINBTS,X'08'       (INCLUDING DELETED RECDS)                    
         GOTO1 SEQ                 SEE IF ITS IN THE SAME FAMILY                
         MVI   ERROR,67                                                         
         NI    DMINBTS,X'FF'-X'08'                                              
         LA    R4,KEYSAVE+14                                                    
         LA    R5,15                                                            
         SPACE 2                                                                
AC38     CLI   0(R4),C' '          FIND LENGTH OF KEY                           
         BNE   AC40                                                             
         BCTR  R4,0                                                             
         BCT   R5,AC38                                                          
         SPACE 2                                                                
AC40     BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   AC41                NOT IN SAME FAMILY                           
         TM    IO+44,X'80'         IF NEXT HIGH IS DELETED,ITS OK               
         BNO   XIT                 NOT OK TO DELETE                             
AC41     MVC   KEY,KEYSAVE         NO LOWER RECORDS                             
         GOTO1 READ                                                             
         LA    RF,IO2                                                           
         OI    44(RF),X'80'        SO OK TO DELETE THIS ONE                     
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
CHECKHIR MVC   KEY,SPACES          CHECK IF HIGHER LEVEL IS THERE               
         MVC   KEY(3),IO2                                                       
         LA    RF,IO2                                                           
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),3(RF)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    XIT                                                              
         MVI   ERROR,NOHIGHER                                                   
         B     XIT                                                              
         SPACE 2                                                                
TOOLON   MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 1                                                                
INVLEDG  MVI   ERROR,9             INVALID LEDGER                               
         B     XIT                                                              
         SPACE 1                                                                
INVINPUT MVI   ERROR,2             INVALID INPUT FIELD                          
         B     XIT                                                              
INVSPLIT MVI   ERROR,127           SPLIT MORE THAN ONCE                         
         B     XIT                                                              
INVWILD  MVI   ERROR,128           SPLIT WITH WILD CARD (C'*')                  
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                  CANNED SPLIT/UNSPLIT DATA ROUTINES           
         SPACE 2                                                                
APGSPLIT NTR1                      SPLIT ACC/CON FROM SCREEN TO DSECT           
*                                  FORMAT....                                   
*                                  R3 CONTAINS SCREEN FIELD ADDR                
*                                  R7 CONTAINS DSECT FIELD ADDR                 
*                                  ROUTINE ALSO VERIFIES LEDGERS ON             
*                                  EACH ACCOUNT PRESENTED, NOT CONTRAS          
*                                  BECAUSE OF STRANGE CONTRAS ON SOME           
*                                  ACCOUNTS USED IN FINANCIAL REPORTING         
         SPACE 1                                                                
         ST    R7,SV7                                                           
         MVI   SPLERR,C' '         RESET ERROR SW                               
         MVI   SPLWILD,NO          NO WILD CARD INPUT                           
         MVI   SPLSPLIT,NO         NO SPLIT ACCOUNT/CONTRA                      
         LA    R1,29(R3)           SAVE EO FIELD ADDR                           
APGS0    LA    R5,15                                                            
APGS1    CLI   0(R3),C'-'          SPLITTER                                     
         BE    APGS2                                                            
         CLI   0(R3),C'*'          WILD CARD ENTRY?                             
         BNE   *+8                                                              
         MVI   SPLWILD,YES                                                      
         CLI   0(R3),X'00'         END OR NOTHING                               
         BE    APGS3                                                            
         MVC   0(1,R7),0(R3)       MOVE ONE CHAR                                
         LA    R7,1(R7)            UP REGS BY ONE                               
         LA    R3,1(R3)                                                         
         CR    R1,R3               EO FIELD                                     
         BE    APGS3                                                            
         BCT   R5,APGS1                                                         
         MVI   SPLERR,C'1'         NO SPLITTER                                  
         B     XIT                                                              
         SPACE 1                                                                
APGS2    CLI   SPLSPLIT,NO                                                      
         BE    APGS2A                                                           
         MVI   SPLERR,C'3'         CAN'T SPLIT TWICE                            
         B     XIT                                                              
         SPACE 1                                                                
APGS2A   MVI   SPLSPLIT,YES                                                     
         L     R7,SV7              GET TO TO FIELD IN DSECT                     
         LA    R7,14(R7)                                                        
         LA    R3,1(R3)            GET PAST SPLITTER                            
         B     APGS0               DO IT ALL AGAIN                              
         SPACE 1                                                                
APGS3    CLI   SPLSPLIT,YES        IS ACCOUNT SPLIT?                            
         BNE   APGS3A              NO                                           
         CLI   SPLWILD,YES         IS ACCOUNT WILD AND SPLIT?                   
         BNE   APGS3A              NO                                           
         MVI   SPLERR,C'4'         YES, THAT'S NOT ALLOWED                      
         B     XIT                                                              
         SPACE 1                                                                
APGS3A   MVC   KEY,SPACES          CHECK LGR(S)                                 
         L     R7,SV7                                                           
         MVC   SPLERR,C' '         RESET ERR SW                                 
         CLI   SPLCA,C'C'          IF CONTRAS DON'T CHECK LGR(S)                
         BE    XIT                                                              
         CLI   0(R7),X'40'         OR ON EMPTIES EITHER                         
         BE    XIT                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),0(R7)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BNE   APGS4                                                            
         MVC   KEY,SPACES                                                       
         CLI   14(R7),X'40'        BYPASS EMPTIES                               
         BE    XIT                                                              
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),14(R7)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BNE   APGS4                                                            
         B     XIT                                                              
APGS4    MVI   SPLERR,C'2'         INDICATE BAD LGR                             
         B     XIT                                                              
         EJECT                                                                  
APGUNSPL NTR1                      UNSPLIT ACC/CON FOR SCREEN DISPLAY           
*                                  R3 CONTAINS SCREEN FIELD                     
*                                  R7 CONTAINS DSECT FIELD                      
*                                  ROUTINE STICKS A DASH BETWEEN FIELDS         
         ST    R7,SV7                                                           
         MVI   SW2,C'1'                                                         
APGU0    LA    R5,14                                                            
         BAS   RE,REVERSSC                                                      
APGU1    CLI   0(R7),X'40'                                                      
         BE    APGU1A                                                           
         CLI   0(R7),X'FF'         SUBSTITUTE FOR BLANK                         
         BNE   *+8                                                              
         MVI   0(R7),X'40'         REMOVE IT                                    
         MVC   0(1,R3),0(R7)                                                    
         LA    R7,1(R7)                                                         
         LA    R3,1(R3)                                                         
         BCT   R5,APGU1                                                         
APGU1A   CLI   SW2,C'2'            SECOND PASS THRU                             
         BE    XIT                                                              
         L     R7,SV7                                                           
         CLI   14(R7),X'40'        EMPTY TO FIELD                               
         BE    XIT                                                              
         MVI   0(R3),C'-'                                                       
         MVI   SW2,C'2'                                                         
         LA    R3,1(R3)                                                         
         LA    R7,14(R7)                                                        
         B     APGU0                                                            
         SPACE 3                                                                
REVERSSC NTR1                      REVERSE SCAN FOR BLANKS                      
*                                  SO DISPLAY DOESN'T TERMINATE                 
*                                  AT EMBEDDED SPACES.  ROUTINE                 
*                                  SUSTITUTES X'FF'S.                           
         LA    R7,13(R7)                                                        
REV1     CLI   0(R7),X'40'                                                      
         BNE   REV2                                                             
         BCTR  R7,0                                                             
         C     R7,SV7                                                           
         BE    XIT                                                              
         B     REV1                                                             
         SPACE 1                                                                
REV2     BCTR  R7,0                                                             
         C     R7,SV7                                                           
         BE    XIT                                                              
         CLI   0(R7),X'40'                                                      
         BNE   REV2                                                             
         MVI   0(R7),X'FF'                                                      
         B     REV2                                                             
         EJECT                                                                  
*              DSECT FOR A LINE ON SCREEN                                       
FIND     DSECT                                                                  
FINACCH  DS    CL8                                                              
FINACC   DS    CL29                                                             
FINFLTH  DS    CL8                                                              
FINFLT   DS    CL5                                                              
FINCONH  DS    CL8                                                              
FINCON   DS    CL29                                                             
FINTYPH  DS    CL8                                                              
FINTYP   DS    CL1                                                              
FINACTH  DS    CL8                                                              
FINACT   DS    CL1                                                              
FINLEN   EQU   *-FIND                                                           
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME1D                                                       
SAVEHEIR DS    CL250                                                            
ERRFLG   DS    C                                                                
       ++INCLUDE ACLFMWORK                                                      
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
VGETTXT  DS    V                                                                
PRELOC   DS    F                                                                
BYTE     DS    CL1                                                              
ELCODE   DS    CL1                                                              
SV7      DS    F                                                                
SVREG    DS    F                                                                
SPLERR   DS    C                                                                
SPLWILD  DS    C                   YES/NO                                       
SPLSPLIT DS    C                   YES/NO                                       
ACCWK    DS    CL29                                                             
SPLCA    DS    C                                                                
SW2      DS    C                                                                
*        ACGENBOTH                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACLFM22   05/01/02'                                      
         END                                                                    
