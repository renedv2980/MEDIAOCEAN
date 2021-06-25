*          DATA SET NEPAY00    AT LEVEL 086 AS OF 11/20/17                      
*PHASE T31300A,+0                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE NEGETPAY                                                               
*INCLUDE NETNET                                                                 
*INCLUDE NETBLRDR                                                               
*INCLUDE SPFMTINO                                                               
*                                                                               
*********************************************************                       
******************    HISTORY ************************                          
* LEVEL 84  CHECKS A0B PROFILE TO SEE IF $0 CLEARENCES                          
*           ALLOWED - IF YES,COMMENT ON $0 ALLOWED                              
*                                                                               
*                                                                               
*********************************************************                       
         TITLE 'NETPAK PAY PROGRAM BASE - T31300'                               
T31300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PAYWRKX-PAYWRKD,**PAY00,RA,RR=RE,CLEAR=YES                       
         LR    R9,RC               R9 POINTS TO GLOBAL WORKING STORAGE          
         USING PAYWRKD,R9                                                       
         ST    RE,BRELO            SAVE BASE RELOCATION                         
         ST    RB,BASE1            SAVE BASE REGISTERS                          
         ST    RA,BASE2                                                         
         ST    RD,AWORK                                                         
*                                                                               
         ST    R1,APARM            PARAMETER LIST FROM MONITOR                  
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
PAY01    L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAUSRID                                                  
         MVC   AGYALPH,TWAAGY                                                   
*                                                                               
PAY02    L     RE,ACOMFACS         OBTAIN COMFACS MODULE ADDRESSES              
         USING COMFACSD,RE                                                      
*                                                                               
         MVC   VDATMGR,CDATAMGR                                                 
         MVC   VCALOV,CCALLOV                                                   
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   FULL,CSWITCH                                                     
         DROP  RE                                                               
*                                                                               
*  TO BYPASS TIMEOUT PROBLEM (TASKNEXT DUMP) UNCOMMENT THE                      
*  GETFACT CALL, THIS WILL UNBOUND THE PAY PROGRAM TO IO'S                      
*                                                                               
*        GOTO1 VGETFACT,DMCB,(X'80',0),F#MIOST                                  
*                                                                               
         L     RF,FULL             A(SWITCH)                                    
         GOTO1 (RF),DMCB,X'FEFFFFFF'                                            
         L     RF,DMCB             A(SYSFACS) VIA SPECIAL FASWITCH CALL         
         L     RF,VSELIST-SYSFACD(RF) RF=A(SELIST)                              
*                                                                               
         USING SELISTD,RF                                                       
         LH    R0,0(RF)            LENGTH OF TABLE ENTRY                        
         L     R1,2(RF)            A(END OF TABLE)                              
         LA    RF,6(RF)            A(FIRST ENTRY)                               
         CLC   =C'NET',SENAME      FIND FIRST NET SYSTEM                        
         BE    *+10                                                             
         BXLE  RF,R0,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
         L     RF,SEPGMS           A(PROGRAM NAME LIST)                         
         DROP  RF                                                               
*                                                                               
         USING PGMLSTD,RF                                                       
         LH    R0,0(RF)            LENGTH OF TABLE ENTRY                        
         L     R1,2(RF)            A(END OF TABLE)                              
         LA    RF,6(RF)            A(FIRST ENTRY)                               
         CLC   =C'NPAY',PGMNAME    MATCH ON PROGRAM NAME?                       
         BE    *+10                                                             
         BXLE  RF,R0,*-10          TRY NEXT TABLE ENTRY                         
         DC    H'0'                                                             
         TM    PGMIND,PGMINOP      HAS PROGRAM BEEN STOPPED?                    
         BZ    PAY03               NO                                           
         MVC   FERN,NBERROR                                                     
         LA    R2,PAYOPTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         DROP  RF                                                               
*                                                                               
PAY03    LA    R2,CORETAB          OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,VCALOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
PAY04    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,PAY04                                                         
*                                                                               
         MVI   DMCB+7,QMSPACK                                                   
         GOTO1 (RF),(R1),0                                                      
         MVC   FULL,DMCB                                                        
*                                                                               
PAY06    LA    R1,BASETAB          SET UP BASE FACILITIES                       
         LA    R0,BASETABC         COUNTER                                      
         LA    RE,BASEFACS         POINTER TO WORKING STORAGE                   
PAY07    L     RF,0(R1)            ADDRESS OF BASE FACILITY                     
         A     RF,BRELO            RELOCATE IT                                  
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT ADDRESS                                 
         LA    RE,4(RE)            NEXT OUTPUT AREA                             
         BCT   R0,PAY07                                                         
*                                                                               
         MVC   VMSPACK,FULL                                                     
*                                                                               
PAY10    LA    R1,COMMIN           LINKAGE TO COMMON ROUTINES                   
         SR    R2,R2                                                            
         LA    R3,BASECOM          OUTPUT ADDRESSES                             
         LA    R0,COMMONS          COUNTER                                      
*                                                                               
PAY11    ST    R1,0(R3)                                                         
         STC   R2,0(R3)                                                         
         LA    R2,4(R2)            BUMP BRANCH INDEX                            
         LA    R3,4(R3)            NEXT OUTPUT AREA                             
         BCT   R0,PAY11                                                         
*                                                                               
PAY12    LA    R2,ADTAB            SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,EXADCONS         COUNTER                                      
         LA    R3,AESTTAB          POINT TO FIRST EXTENDED ADCON                
*                                                                               
PAY13    ICM   R1,7,0(R2)          GET DISPLACEMENT                             
         LA    R1,PAYWRKD(R1)      INDEX INTO STORAGE                           
         ST    R1,0(R3)                                                         
         LA    R2,L'ADTAB(R2)      NEXT TABLE ENTRY                             
         LA    R3,4(R3)            NEXT WORKING STORAGE FIELD                   
         BCT   R0,PAY13                                                         
*                                                                               
PAY15    L     R7,ASPOOLA          R7 POINTS TO SPOOL AREA                      
         USING SPOOLD,R7                                                        
         MVI   SPACES,C' '         SET OTHER WORKING STORAGE VALUES             
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         GOTO1 VDATCON,DMCB,(5,0),TODAY                                         
         GOTO1 (RF),(R1),TODAY,(2,TODAYC)                                       
         MVC   UNTFILE,=CL8'UNTFILE'                                            
         MVC   XTRA,SPACES                                                      
         MVC   PAYMSG,SPACES                                                    
         OI    PAYMSGH+6,X'80'                                                  
*                                                                               
PAY20    GOTO1 VCALOV,DMCB,(X'01',0),ATWA                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             GET V(SUBSIDIARY ROOT)                       
         GOTO1 (RF),(R1),PAYWRKD                                                
         EJECT                                                                  
* INTERFACE TO OVERLAY                                                          
*                                                                               
GO       CLI   SEQUENCE,0          TEST FOR USER SUPPLIED SEQUENCE              
         BNE   *+12                YES                                          
         MVI   SEQUENCE,C'D'       DEFAULT IS DATE SEQUENCE                     
         MVI   NBSEQ,C'D'                                                       
         MVI   NBSELMOD,NBVALPRD   VALIDATE PRODUCT                             
         XC    NBSELPNM,NBSELPNM                                                
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    GO0                                                              
         MVC   XTRA(3),NBSELPRD    DISPLAY PRODUCT CODE IN ERROR                
         MVC   FERN,NBERROR                                                     
         LA    R2,PAYOPTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
GO0      L     R4,NBAIO                                                         
         USING PRDHDRD,R4                                                       
         MVC   PRDNAME,PNAME       EXTRACT PRODUCT NAME                         
*                                                                               
         MVI   NBSELMOD,NBVALEST   ALWAYS VALIDATE ESTIMATE                     
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    GO1                                                              
         MVI   FERN,ESTERR                                                      
         LA    R2,PAYOPTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
GO1      CLI   ESTTYP,ISINGLE      TEST FOR ONE ESTIMATE                        
         BNE   GO2                 NO                                           
         L     R4,NBAIO                                                         
         USING ESTHDRD,R4                                                       
         MVC   ENAME,EDESC         EXTRACT ESTIMATE NAME                        
         DROP  R4                                                               
         SPACE                                                                  
GO2      MVC   NBSELSTR,START      SEED PAY PERIOD                              
         MVC   NBSELEND,END                                                     
         MVC   PREOPT,NBUSER+13    SAVE USER PRE-EMPT OPTION                    
         MVI   NBUSER+13,NO        FORCE PRE-EMPTS FOR INTERNAL USE             
         MVI   NBDATA,C'U'                                                      
         MVI   NBSELMOD,NBPROCUN                                                
         SPACE                                                                  
GO4      CLC   SCREEN,LSCREEN      TEST IF SCREEN ALREADY LOADED                
         BE    GO6                 YES                                          
         OI    MODE,FIRST          SET FIRST TIME FOR SCREEN                    
         CLI   SCREEN,0            TEST FOR ACTION W/O SCREEN                   
         BE    GO6                 YES                                          
         MVC   DMCB+4(3),=X'D90313'                                             
         MVC   DMCB+7(1),SCREEN    SCREEN NUMBER                                
         GOTO1 VCALOV,DMCB,PAYLAST                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LSCREEN,SCREEN      UPDATE LAST SCREEN                           
         SPACE 1                                                                
GO6      LA    R1,THISLEN          CONTROL VALUES LENGTH                        
         CLI   ACTION,PRINT                                                     
         BNE   *+8                                                              
         LA    R1,CLEARLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISVALS(0),LASTVALS                                             
         BE    *+8                                                              
         OI    MODE,DISPLAY        FORCE DISPLAY MODE                           
         MVC   LASTVALS(LASTLEN),THISVALS UPDATE LAST TIME CONTROLS             
         MVC   LACT,ACTION         UPDATE LAST ACTION                           
         SPACE 1                                                                
GO8      CLI   ACTION,CLEAR        FOR CLEAR OR TEST, FORCE                     
         BE    *+12                DISPLAY OF BASE SCREEN DATA                  
         CLI   ACTION,TEST         WHEN CONTROL VALUES HAVE CHANGED             
         BNE   GO10                                                             
         TM    MODE,DISPLAY        TEST IF CONTROL VALUES CHANGED               
         BZ    GO9                 NO                                           
         LA    R2,CLRINV1H         SET CURSOR POSITION                          
         ST    R2,FADDR                                                         
         OI    6(R2),X'01'         XMIT BACK MODIFIED FOR RE-ENTRY              
         MVC   PAYMSG(37),=C'ENTER INVOICES, AMOUNTS, AND COMMENTS'             
         B     GOX                                                              
         SPACE 1                                                                
GO9      BAS   RE,EDITCL                                                        
         SPACE 1                                                                
GO10     GOTO1 VCALOV,DMCB,(OVERLAY,0),ATWA                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             OVERLAY ADDRESS                              
         GOTO1 (RF),DMCB,PAYWRKD                                                
         SPACE 1                                                                
GOX      NI    MODE,X'FF'-FIRST-DISPLAY MAKE SURE FIRST AND DIS IS OFF          
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO EDIT CLEARANCE SCREEN AND BUILD TABLE OF INVOICE,              
* AMOUNT, AND COMMENT FIELDS PRESENT                                            
*                                                                               
EDITCL   NTR1                                                                   
         XC    AMTS,AMTS                                                        
         LA    R5,AMTS                                                          
         LA    R2,CLRINV1H                                                      
*                                                                               
         CLC   =C'CLAPY',PAYACT    FROM AUTOPAY?                                
         BNE   EDIT2                                                            
         BAS   RE,CHKAPY                                                        
*                                                                               
* GO DOWN THE SCREEN AND NOTE FIELDS THAT HAVE BEEN INPUT                       
*                                                                               
EDIT2    GOTO1 VGETFLD,(R2)                                                     
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    *+8                                                              
         OI    7(R5),X'80'         X'80' = INVOICE PRESENT                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VGETFLD,(R2)                                                     
         CLI   FLDH+5,0                                                         
         BE    *+8                                                              
         OI    7(R5),X'40'         X'40' = AMOUNT PRESENT                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VGETFLD,(R2)                                                     
         CLI   FLDH+5,0                                                         
         BE    *+8                                                              
         OI    7(R5),X'20'         X'20' = COMMENT PRESENT                      
*                                                                               
         LA    R5,8(R5)                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,CLRCOMNH         POINT TO LAST FIELD                          
         CR    R2,R0                                                            
         BNH   EDIT2                                                            
         SPACE                                                                  
* CHECK THAT FIRST LINE HAS AN INVOICE AND AMOUNT                               
*                                                                               
EDIT4    LA    R5,AMTS                                                          
         LA    R2,CLRINV1H                                                      
         MVI   FERN,MISERR                                                      
         TM    7(R5),X'80'         LINE 1 MUST HAVE INV                         
         BZ    EDITR                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    7(R5),X'40'          AND AMOUNT                                  
         BZ    EDITR                                                            
         SPACE 1                                                                
* EDIT AMOUNT FIELD                                                             
*                                                                               
EDIT10   GOTO1 VGETFLD,(R2)                                                     
         MVI   4(R5),C'1'          SET AMT TYPE                                 
         TM    7(R5),X'40'         TEST AMOUNT PRESENT                          
         BZ    EDIT15              NO - DONT EDIT                               
         ZIC   R0,FLDH+5                                                        
         CLI   FLDH+5,2                                                         
         BNH   EDIT14                                                           
* CHECK FOR CR OR CK AT END OF FIELD                                            
         LA    RE,FLD                                                           
         AR    RE,R0                                                            
         SH    RE,=H'2'                                                         
*                                                                               
*****    CLC   =C'CR',0(RE)       CR/CK MUST BE ON FIRST LINE                   
*****    BE    *+14                                                             
*****    CLC   =C'CK',0(RE)                                                     
*****    BNE   EDIT10C                                                          
*****    MVI   FERN,USERERR                                                     
*****    XC    PAYMSG,PAYMSG                                                    
*****    MVC   PAYMSG(L'CRCKERR),CRCKERR                                        
*****    LA    RF,AMTS            ON THE FIRST LINE ?                           
*****    CR    RF,R5                                                            
*****    BNE   ERROR              NOT ON 1ST LINE-ERROR                         
*****    MVI   FERN,0                                                           
*****    XC    PAYMSG,PAYMSG                                                    
*                                                                               
EDIT10C  CLC   =C'CR',0(RE)                                                     
         BNE   EDIT11                                                           
         MVI   BYTE2,C'A'                                                       
         OC    CRCKDAT,CRCKDAT     IF REVERSE= OPTION                           
         BZ    *+8                                                              
         OI    PAYFLAGS,REVRSCR    SET FLAG                                     
         MVI   4(R5),C'2'                                                       
         TM    NBPPDOPT,NBPPDCK    ALREADY A CASH RECEIPT?                      
         BO    *+8                                                              
         OI    NBPPDOPT,NBPPDCR                                                 
         B     EDIT12                                                           
*                                                                               
EDIT11   CLC   =C'CK',0(RE)                                                     
         BNE   EDIT14                                                           
         MVI   BYTE2,C'A'                                                       
         OC    CRCKDAT,CRCKDAT     IF REVERSE= OPTION                           
         BZ    *+8                                                              
         OI    PAYFLAGS,REVRSCK    SET FLAG                                     
         MVI   4(R5),C'3'                                                       
         TM    NBPPDOPT,NBPPDCR    ALREADY A CREDIT?                            
         BO    *+8                                                              
         OI    NBPPDOPT,NBPPDCK                                                 
EDIT12   SH    R0,=H'2'            ADJUST LEN FOR CR/CK                         
*                                                                               
EDIT14   MVI   FERN,INVERR                                                      
         GOTO1 VCASHVAL,DMCB,(2,FLD),(R0)                                       
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BM    ERROR                                                            
         ST    R0,0(R5)            SAVE AMOUNT                                  
                                                                                
* CHECK A0B PROFILE TO SEE IF PROFILE ALLOWS $0 CLEARANCES                      
* IF IT DOES NOT, THEN COMMENT NOT ALLOWED ON $0 CLEARANCE                      
         OC    0(4,R5),0(R5)       TEST AMOUNT = 0                              
         BNZ   EDIT15                                                           
         TM    7(R5),X'20'         IS THERE A COMMENT ?                         
         BNO   EDIT15              NO                                           
         CLI   PAYPROFB+3,C'Y'    A0B PROFILE ALLOWS $0 CLEARANCES              
         BE    EDIT15             YES - ALLOW COMMENT ON $0 CLEARANCE           
         ZIC   R0,0(R2)           NO - BUMP TO COMMENT LINE                     
         AR    R2,R0                                                            
         ST    R2,FADDR                                                         
         MVI   FERN,INVERR                                                      
         B    ERROR                    DO NOT ALLOW COMMENT ON $0               
                                                                                
***      LA    R0,AMTS                                                          
***      CR    R0,R5               TEST LINE 1                                  
***      BNE   EDIT14X             NO                                           
***      OC    0(4,R5),0(R5)       TEST AMOUNT = 0                              
***      BNZ   EDIT14X                                                          
***      B     EDIT25              MAKE SURE NO MORE DATA                       
*                                                                               
***EDIT14X  OC    0(4,R5),0(R5)    ZERO AMOUNT NOT ON LINE 1 IS ERROR           
***      BZ    ERROR                                                            
*                                                                               
EDIT15   DS    0H                                                               
***      CLI   ACTION,TEST                                                      
***      BE    EDIT15A                                                          
***      CLI   BYTE2,C'A'          IF CR/CK                                     
***      BNE   EDIT15A                                                          
***      MVI   FERN,USERERR                                                     
***      XC    PAYMSG,PAYMSG                                                    
***      MVC   PAYMSG(L'CRCKERR),CRCKERR                                        
****     OC    CRCKDAT,CRCKDAT     IF REVERSE OPTION                            
****     BZ    EDIT15A                                                          
****     TM    PAYFLAGS,X'06'      MUST BE CR OR CK                             
****     BZ    ERROR                                                            
***      ZIC   R0,0(R2)            BUMP OVER COMMENT LINE                       
***      AR    R2,R0                                                            
***      B     EDIT25              MUST BE NO MORE DATA                         
*                                                                               
EDIT15A  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT INV                                     
         LA    R0,CLRINVNH         LAST INVOICE                                 
         CR    R2,R0                                                            
         BH    EDIT30                                                           
*                                                                               
         TM    7(R5),X'20'         THIS LINE HAVE COMMENT                       
         BO    EDIT20                                                           
* NO COMMENT THIS LINE                                                          
         LA    R5,8(R5)                                                         
         CLI   7(R5),0             TEST DATA THIS LINE                          
         BE    EDIT25              NO                                           
EDIT16   MVI   FERN,MISERR                                                      
         TM    7(R5),X'80'         TEST INV NEXT LINE                           
         BZ    EDITR                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    7(R5),X'40'         TEST AMT NEXT LINE                           
         BZ    EDITR                                                            
         B     EDIT10                                                           
*                                                                               
* COMMENT THIS LINE                                                             
*                                                                               
EDIT20   LA    R5,8(R5)                                                         
         CLI   7(R5),0             TEST DATA NEXT LINE                          
         BE    EDIT25              NO                                           
         TM    7(R5),X'C0'         TEST INV OR AMT NEXT LINE                    
         BNZ   EDIT16              YES-SHOULD HAVE BOTH                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO AMT                                 
         B     EDIT15                                                           
         SPACE 2                                                                
* NO DATA THIS LINE - SHOULD HAVE NO MORE DATA                                  
*                                                                               
EDIT25   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,CLRCOMNH                                                      
         CR    R2,R0                                                            
         BH    EDIT30                                                           
         GOTO1 VGETFLD,(R2)                                                     
         CLI   FLDH+5,0                                                         
         BE    EDIT25                                                           
****     CLI   BYTE2,C'A'          CRCK?                                        
****     BNE   EDIT25D                                                          
****     MVI   FERN,USERERR                                                     
****     XC    PAYMSG,PAYMSG                                                    
****     MVC   PAYMSG(L'CRCKERR),CRCKERR                                        
****     B     *+8                                                              
EDIT25D  MVI   FERN,INVERR                                                      
         B     ERROR                                                            
         SPACE 1                                                                
* SUM AMOUNTS AND SAVE TOTAL                                                    
*                                                                               
EDIT30   LA    R0,CLRLINES         COUNTER OF LINES ON SCREEN                   
         LA    R5,AMTS                                                          
         SR    RE,RE                                                            
EDIT40   L     RF,0(R5)                                                         
         CLI   4(R5),C'1'                                                       
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         LA    R5,8(R5)                                                         
         BCT   R0,EDIT40                                                        
*                                                                               
         ST    RE,TOTAMT                                                        
         S     RE,=F'50'                                                        
         ST    RE,TOTAMTLO                                                      
         A     RE,=F'100'                                                       
         ST    RE,TOTAMTHI                                                      
         B     EXXMOD                                                           
         SPACE 2                                                                
EDITR    ST    R2,FADDR                                                         
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK IF COMING FROM AUTOPAY                                                  
***********************************************************************         
CHKAPY   NTR1                                                                   
         MVI   APYFLAG,0                                                        
         XC    APYKEY,APYKEY                                                    
         MVC   APYKEY(2),=X'0D3F'                                               
*                                                                               
         LA    R3,CLRINV1H                                                      
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         CLC   =C'SCR=APY',8(R3)                                                
         BNE   EXXMOD                                                           
*                                                                               
         XC    8(L'CLRINV1,R3),8(R3)                                            
         MVI   5(R3),0                                                          
*                                                                               
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         MVC   WORK,8(R3)                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,WORK,APYKEY+2,30                                       
*                                                                               
         XC    8(L'CLRCOM1,R3),8(R3)                                            
         MVI   5(R3),0                                                          
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         CLC   =C'SCR=APY',8(R3)                                                
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    8(L'CLRINV1,R3),8(R3)                                            
         MVI   5(R3),0                                                          
*                                                                               
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         MVC   WORK,8(R3)                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,WORK,APYKEY+17,30                                      
*                                                                               
         XC    8(L'CLRCOM1,R3),8(R3)                                            
         MVI   5(R3),0                                                          
*                                                                               
         OI    APYFLAG,APYYES                                                   
CHKAPYX  B     EXXMOD                                                           
*                                                                               
* LINKAGE TO COMMON SUB-ROUTINES                                                
*                                                                               
COMMIN   NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,ASPOOLA          RESTORE ADDRESSABILITY TO SPOOL              
         SRL   RF,24               SHIFT BRANCH INDEX TO L.O.B.                 
         B     COMTAB(RF)                                                       
         SPACE 1                                                                
COMTAB   B     GETFLD                                                           
         B     ERROR                                                            
         B     EXIT                                                             
         B     CLEARF                                                           
         B     FILREC                                                           
         B     DISFIL                                                           
         B     GETRSN                                                           
         DC    6A(0)                                                            
COMMONS  EQU   (*-COMTAB)/4                                                     
         EJECT                                                                  
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY R1 POINTS TO FIELD HEADER                                            
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLD   ST    R1,FADDR                                                         
         LR    R2,R1                                                            
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GETFLDX                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    GETFLDX                                                          
         CLI   FLDH+5,15           NO MORE THAN 15 DIGITS                       
         BH    GETFLD4                                                          
         BCTR  R1,0                                                             
         EX    R1,FLDPACK                                                       
         CP    DUB,=P'2147000000'  TEST FOR FULLWORD MAX                        
         BH    GETFLD4             TREAT IT AS NON-NUMERIC                      
         CVB   R0,DUB                                                           
         B     GETFLDX                                                          
         SPACE 1                                                                
GETFLD4  NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
         SPACE 1                                                                
GETFLDX  XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
FLDPACK  PACK  DUB,FLD(0)                                                       
         EJECT                                                                  
* ERROR - SET ERROR MESSAGE AND EXIT                                            
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER OF FIELD IN ERROR)                              
*        FERN  = SYSTEM ERROR NUMBER OR X'FF' FOR USER MESSAGE                  
*        FNDX  = OPTIONALLY SET FIELD INDEX FOR MULTI-FIELD TWA FIELD           
*        XTRA  = OPTIONALLY CONTAINS EXTRA MESSAGE CONCATENATED TO              
*                SYSTEM ERROR MESSAGE                                           
*                                                                               
ERROR    CLI   FERN,USERERR        TEST FOR USER MESSAGE                        
         BE    ERROR6                                                           
         GOTO1 VGETMSG,DMCB+12,(FERN,PAYMSG),(X'FF',DMCB),(7,0)                 
         LA    R2,PAYMSG+L'PAYMSG-1 R2 POINTS TO END OF MSG FLD                 
         LA    R3,L'PAYMSG         CALCULATE MESSAGE LENGTH                     
         CLI   0(R2),C' '          TEST FOR BLANK                               
         BNE   *+10                                                             
         BCTR  R2,0                BACK UP POINTER                              
         BCT   R3,*-10                                                          
         LA    R2,1(R2)            POINT TO BLANK AFTER LAST CHAR               
*                                                                               
ERROR2   CLI   FNDX,0              TEST FOR INDEX NUMBER                        
         BE    ERROR4                                                           
         LA    R0,L'PAYMSG                                                      
         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1                
         CR    R3,R0               TEST FOR FIT IN FIELD                        
         BH    ERROR6              NO - EXIT                                    
         LA    R2,PAYMSG-7(R3)                                                  
         MVC   0(7,R2),=C'- FLD#N'                                              
         EDIT  (B1,FNDX),(1,6(R2))                                              
         LA    R2,7(R2)            POINT TO BLANK AFTER INDEX MSG               
*                                                                               
ERROR4   CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                   
         BE    ERROR6                                                           
         LA    RE,XTRA+L'XTRA-1                                                 
         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LA    R0,L'PAYMSG                                                      
         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH                 
         CR    R3,R0               TEST FOR FIT                                 
         BH    ERROR6                                                           
         BCTR  R1,0                LESS ONE FOR EXECUTE                         
         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                   
         B     ERROR6                                                           
         MVC   1(0,R2),XTRA        EXECUTED                                     
*                                                                               
ERROR6   B     EXIT                                                             
         EJECT                                                                  
* EXIT - SET CURSOR AND EXIT DIRECTLY TO USER                                   
*                                                                               
EXIT     L     R2,FADDR            SET CURSOR                                   
         OI    6(R2),X'C0'         CURSOR AND TRANSMIT BITS                     
         L     RD,AWORK            RESTORE ROOT'S RD                            
         B     EXXMOD                                                           
         EJECT                                                                  
* CLEARF - CLEAR AND FOUT FIELDS                                                
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLEARF   LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     EXXMOD              YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
* FILREC - APPLY RECORD FILTERS NOT SUPPORTED BY NETIO                          
*                                                                               
* ON ENTRY, ASSUMES NETBLOCK POINTS TO AND DESCRIBES RECORD                     
* ON EXIT, CC=EQ FOR ACCEPT RECORD, NEQ FOR SKIP RECORD                         
*                                                                               
FILREC   DS    0H                                                               
         L     RF,NBACLI                                                        
         CLI   1(RF),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   UFILTER,SPACES                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',NBAIO),(1,=C'P')               
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   FILREC1             YES                                          
         L     R5,12(R1)                                                        
         USING NUFILD,R5                                                        
         MVC   UFILTER,NUFILTER    EXTRACT RECORD FILTER                        
         SPACE 1                                                                
FILREC1  CLI   SUB,0               TEST FOR SUB-LINE                            
         BE    *+14                NO                                           
         CLC   SUB,NBACTSUB        TEST AGAINST RECORD SUB-LINE                 
         BNE   SKIPREC                                                          
*                                                                               
         OC    P1PCT,P1PCT         TEST FOR 1'ST PRODUCT PERCENT                
         BZ    *+14                NO                                           
         CLC   P1PCT,NBP1SHR       TEST AGAINST RECORD PROD PCT.                
         BNE   SKIPREC                                                          
*                                                                               
         CLI   PRDLKSW,X'FF'       SHOULD PRODUCT BE CHECKED                    
         BNE   *+12                                                             
         BAS   RE,CHKPROD                                                       
         BNE   SKIPREC                                                          
*                                                                               
         CLI   NOMTCH,C'Y'         SEE IF MATCH REQUIRED FOR PAYING             
         BE    FILREC1M                                                         
         CLI   PAYPROF+3,C'Y'      SEE IF MATCH REQUIRED FOR PAYING             
         BNE   FILREC1M                                                         
         OC    NBAFFTIM,NBAFFTIM   CHECK FOR AFFIDAVIT TIME                     
         BNZ   FILREC1A                                                         
         OC    NBSDAFDT,NBSDAFDT   CHECK FOR AFFIDAVIT DATE                     
         BNZ   FILREC1A                                                         
         B     FILREC1K                                                         
*                                                                               
*  CHECK INVOICE NUMBERS AS OF (APR1/04)                                        
*                                                                               
FILREC1A CLC   NBACTDAT,=XL2'D081'     APR1/04                                  
         BL    FILREC1M                                                         
*                                                                               
         XC    BYTE,BYTE                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   FILREC1C            YES                                          
         L     RE,12(R1)                                                        
         USING NUDTAD,RE                                                        
         OC    NUDTINVN,NUDTINVN                                                
         BZ    *+8                                                              
         OI    BYTE,X'01'                                                       
         OC    NUDTIIVN,NUDTIIVN                                                
         BZ    *+8                                                              
         OI    BYTE,X'02'                                                       
*  CLEAR BOTH TIME AND INTEGRATION                                              
FILREC1C CLI   PAYTYPE,C'Z'                                                     
         BNE   FILRE1C5                                                         
*                                                                               
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
*                                                                               
         CLC   NUKNET,=C'ABC '     IF ABC, CBS, OR NBC, CHECK BOTH              
         BE    FILRE1C5            TIME AND INTEGRATION ELSE, JUST              
         CLC   NUKNET,=C'NBC '     CHECK TIME.                                  
         BE    FILRE1C5                                                         
         CLC   NUKNET,=C'CBS '                                                  
         BNE   FILRE1D5                                                         
         DROP  RF                                                               
*                                                                               
FILRE1C5 CLI   PAYTYPE,0                                                        
         BNE   FILREC1D                                                         
         CLI   PAYTYPE,C'Z'                                                     
         BNE   FILREC1D                                                         
         TM    BYTE,X'03'          BOTH INVOICES MUST BE PRESENT                
         BO    FILREC1M                                                         
         B     FILREC1K                                                         
*  CLEAR TIME                                                                   
FILREC1D CLI   PAYTYPE,C'T'                                                     
         BNE   FILREC1E                                                         
FILRE1D5 TM    BYTE,X'01'          TIME INVOICE MUST BE PRESENT                 
         BO    FILREC1M                                                         
         B     FILREC1K                                                         
*  CLEAR INTEGRATION                                                            
FILREC1E CLI   PAYTYPE,C'I'                                                     
         BNE   FILREC1M                                                         
         TM    BYTE,X'02'          INTG INVOICE MUST BE PRESENT                 
         BO    FILREC1M                                                         
         B     FILREC1K                                                         
         DROP  RE                                                               
*                                                                               
*                                                                               
*  IF UNIT PRE-EMPTED OR MISSED AND THERE IS NO PAY INFO                        
*  BYPASS THE MATCH CHECK                                                       
*                                                                               
FILREC1K GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),(1,=C'P')               
         CLI   12(R1),0            TEST IF PAY ELEMENT FOUND                    
         BE    FILREC1N            YES SET NO MATCH ERROR                       
         TM    NBUNITST,X'40'      IS UNIT PRE-EMPTED                           
         BNZ   FILREC1M            YES, BYPASS MATCH CHECK                      
         TM    NBUNITST,X'02'      IS UNIT MISSED                               
         BNZ   FILREC1M            YES, BYPASS MATCH CHECK                      
FILREC1N MVI   MATCHSW,X'FF'       SET UNMATCHED DATA SWITCH                    
         B     SKIPREC                                                          
*                                                                               
FILREC1M TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    FILREC1O            YES                                          
         CLC   SPECREP,NBSREP      TEST FOR MATCH ON SPECIAL REP                
         BE    FILREC2             YES                                          
*                                                                               
         BAS   RE,GETSPRAT         CHECK SREP AGAINST S-RATE ELEMENTS           
         L     RF,NBACLI                                                        
         CLI   1(RF),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   DMCB+12,0           GOOD RETURN CODE                             
         BNE   SKIPREC             NO-SKIP RECORD                               
         B     FILREC2             YES                                          
*                                                                               
FILREC1O OC    NBSREP,NBSREP       TEST FOR ANY SPECIAL REP                     
         BNZ   FILREC2             NO-OMIT RECORD                               
*                                                                               
         BAS   RE,GETSPRAT         CHECK SREP AGAINST S-RATE ELEMENTS           
         L     RF,NBACLI                                                        
         CLI   1(RF),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   DMCB+12,0           GOOD RETURN CODE                             
         BNE   SKIPREC             NO-SKIP RECORD                               
         B     FILREC2             YES                                          
         SPACE 1                                                                
FILREC2  OC    PROD,PROD           TEST FOR PRODUCT FILTER                      
         BZ    FILREC3             NO                                           
         BAS   RE,CHKPROD                                                       
         BNE   SKIPREC                                                          
*                                                                               
FILREC3  OC    FILTER,FILTER       TEST IF FILTER PRESENT                       
         BZ    FILREC6             NO                                           
*                                                                               
         LA    R0,L'NUFILTER       SET R0 AS A COUNTER                          
         MVC   DUB(L'FILTER),FILTER                                             
         LA    R2,DUB              R2 POINTS TO USER FILTER                     
         LA    R3,UFILTER          R3 POINTS TO RECORD FILTER                   
         SPACE                                                                  
FILREC4  LA    R1,X'80'            SET BRANCH MASK TO EQUAL                     
         TM    0(R2),X'40'         TEST FOR NEGATIVE FILTER                     
         BO    *+12                NO                                           
         LA    R1,X'70'            YES-SET BRANCH TO NEQ                        
         OI    0(R2),X'40'         RESTORE UPPER CASE                           
         CLC   0(1,R2),0(R3)       USER VS. RECORD FILTER                       
         EX    R1,FILBRAN                                                       
         B     SKIPREC                                                          
*                                                                               
FILREC5  LA    R2,1(R2)            BUMP USER FILTER POINTER                     
         LA    R3,1(R3)            BUMP RECORD FILTER POINTER                   
         BCT   R0,FILREC4                                                       
         SPACE                                                                  
FILREC6  CLC   BILLST,=2X'00'      TEST FOR BILLING STATUS FILTER               
         BE    FILREC8             NO                                           
         TM    BILLST+1,TMBIL+TMUNBIL                                           
         BZ    FILREC7             NEITHER                                      
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',NBAIO),0                       
         LA    R2,X'80'                                                         
         TM    BILLST+1,TMBIL                                                   
         BO    *+8                                                              
         LA    R2,X'70'                                                         
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         EX    R2,FILBRAN2                                                      
         B     SKIPREC                                                          
*                                                                               
FILREC7  MVI   BYTE,C'T'                                                        
         TM    BILLST+1,TMTBIL     TEST FOR TIME BILLED                         
         BO    FILREC7A                                                         
*                                                                               
         MVI   BYTE,C'I'                                                        
         TM    BILLST+1,TMINTBIL   TEST FOR INTEGRATION BILLED                  
         BO    FILREC7A                                                         
*                                                                               
         MVI   BYTE,C'U'                                                        
         TM    BILLST+1,TMCUTBIL   TEST FOR CUT-IN BILLED                       
         BO    FILREC7A                                                         
*                                                                               
         MVI   BYTE,C'B'                                                        
         TM    BILLST+1,TMBLKBIL   TEST FOR BLACKOUT BILLED                     
         BO    FILREC7A                                                         
*                                                                               
         MVI   BYTE,C'S'                                                        
         TM    BILLST+1,TMCOPBIL   TEST FOR COPY-SPLIT BILLED                   
         BO    FILREC7A                                                         
*                                                                               
         MVI   BYTE,C'O'                                                        
FILREC7A GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',NBAIO),(1,BYTE)                
         CLI   12(R1),0                                                         
         BE    FILREC8                                                          
         B     SKIPREC                                                          
         SPACE                                                                  
FILREC8  CLC   CLEARST(2),=2X'00'      TEST FOR CLEARED FILTER                  
         BE    FILREC10            NO-ALL DONE                                  
         TM    CLEARST+1,TMPAI+TMUNPAI+TMPARPAI                                 
         BZ    FILREC9                                                          
         LA    R2,X'70'            BRANCH CONDITION FOR PAID/PART               
         TM    CLEARST+1,TMPAI+TMPARPAI                                         
         BNZ   *+8                                                              
         LA    R2,X'80'            UNPAID                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),0                       
         CLI   12(R1),0                                                         
         EX    R2,FILBRAN3                                                      
         TM    CLEARST+1,TMPARPAI     TEST FOR PARTIAL FILTER                   
         BZ    FILREC10            NO-RECORD PASSES                             
*                                                                               
         CLC   NBACTUAL,NBPAYTGR   TEST IF ACTUAL ALL PAID                      
         BNE   FILREC10            NP-ITS PARTIALLY PAID                        
         CLC   NBINTEG,NBPAYIGR                                                 
         BNE   FILREC10                                                         
         B     SKIPREC             ALL PAID                                     
*                                                                               
FILREC9  MVI   BYTE,C'T'                                                        
         TM    CLEARST+1,TMTPAI    TEST FOR TIME PAID                           
         BO    FILREC9A                                                         
*                                                                               
         MVI   BYTE,C'I'                                                        
         TM    CLEARST+1,TMINTPAI  TEST FOR INTEGRATION PAID                    
         BO    FILREC9A                                                         
*                                                                               
         MVI   BYTE,C'U'                                                        
         TM    CLEARST+1,TMCUTPAI  TEST FOR CUT-IN PAID                         
         BO    FILREC9A                                                         
*                                                                               
         MVI   BYTE,C'B'                                                        
         TM    CLEARST+1,TMBLKPAI  TEST FOR BLACKOUT PAID                       
         BO    FILREC9A                                                         
*                                                                               
         MVI   BYTE,C'S'                                                        
         TM    CLEARST+1,TMCOPPAI  TEST FOR COPY-SPLIT PAID                     
         BO    FILREC9A                                                         
*                                                                               
         MVI   BYTE,C'O'                                                        
FILREC9A GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),(1,BYTE)                
         CLI   12(R1),0                                                         
         BE    FILREC10                                                         
         B     SKIPREC                                                          
*                                                                               
FILREC10 OC    ACTOPT,ACTOPT       TEST FOR ACTUAL COST FILTER                  
         BZ    FILREC12            NO                                           
         CLC   ACTOPT,NBACTUAL     IF NE BYPASS RECORD                          
         BNE   SKIPREC                                                          
*                                                                               
FILREC12 OC    SVCOML,SVCOML       TEST FOR COMMERCIAL FILTER                   
         BZ    FILREC15                                                         
         L     RE,NBAIO                                                         
         LA    RE,27(RE)                                                        
CML100   CLI   0(RE),X'24'                                                      
         BE    CML200                                                           
*                                                                               
CML120   SR    RF,RF                                                            
         ICM   RF,1,1(RE)                                                       
         BZ    SKIPREC                                                          
         AR    RE,RF                                                            
         B     CML100                                                           
*                                                                               
CML200   CLC   NBSPLPRN,2(RE)      EQUAL PRODUCT                                
         BNE   CML120                                                           
         CLC   SVCOML,3(RE)                                                     
         BNE   CML120                                                           
         B     OKREC                                                            
*                                                                               
FILREC15 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),0                       
         CLI   12(R1),0                                                         
         BE    FILREC16            UNIT PAID OK                                 
         TM    NBUNITST,X'40'      WAS UNIT PRE-EMPTED                          
         BO    SKIPREC                                                          
*                                                                               
FILREC16 DS    0H                                                               
         OC    INVNUM,INVNUM       FILTER BY INVOICE #?                         
         BZ    FILREC20                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   SKIPREC             YES                                          
         L     R5,12(R1)                                                        
         USING NUDTAD,R5                                                        
*                                                                               
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
*                                                                               
         CLC   NUKNET,=C'ABC '     IF ABC, CBS, OR NBC, CHECK BOTH              
         BE    FILR17A             TIME AND INTEGRATION ELSE, JUST              
         CLC   NUKNET,=C'NBC '     CHECK TIME.                                  
         BE    FILR17A                                                          
         CLC   NUKNET,=C'CBS '                                                  
         BNE   FILR18A                                                          
         DROP  RF                                                               
*                                                                               
FILR17A  CLI   PAYTYPE,0           BOTH TIME AND INTEGRATION?                   
         BE    *+12                DEFAULT FOR ABC/CBS/NBC                      
         CLI   PAYTYPE,C'Z'                                                     
         BNE   FILR18B                                                          
*                                                                               
         CLC   NUDTINVN,INVNUM     MUST BE TIME AND INT MATCH                   
         BNE   SKIPREC                                                          
         CLC   NUDTIIVN,INVNUM                                                  
         BNE   SKIPREC                                                          
         B     FILREC20                                                         
*                                                                               
FILR18A  CLI   PAYTYPE,0           DEFAULT IS TIME FOR EVERYBODY ELSE           
         BE    FILR18B2                                                         
         CLI   PAYTYPE,C'Z'                                                     
         BE    FILR18B2                                                         
*                                                                               
FILR18B  CLI   PAYTYPE,C'T'        PAY BY TIME?                                 
         BNE   FILR18C                                                          
FILR18B2 CLC   NUDTINVN,INVNUM                                                  
         BE    FILREC20                                                         
         B     SKIPREC                                                          
*                                                                               
FILR18C  CLI   PAYTYPE,C'I'        PAY BY INT?                                  
         BNE   SKIPREC                                                          
         CLC   NUDTIIVN,INVNUM     MATCH ON INTEGRATION INVOICE?                
         BNE   SKIPREC                                                          
*                                                                               
FILREC20 DS    0H                                                               
         OC    PGRP,PGRP           ANY PRODUCT GROUP FILTER?                    
         BZ    FILREC30                                                         
*                                                                               
         BAS   RE,BLDPRLST         BUILD TABLE W/ UNIT'S PRODUCTS               
         CLI   WORK,X'FF'          IS THIS UNIT ALLOCATED?                      
         BE    SKIPREC             NO - SKIP IT                                 
*                                                                               
         LA    R5,WORK                                                          
*                                                                               
FILREC21 DS    0H                                                               
         CLI   0(R5),X'FF'         FINISHED ALL PRODUCTS?                       
         BE    SKIPREC                                                          
*                                                                               
         XC    KEY,KEY             CHECK IF PRODUCT IS IN PGROUP                
         LA    R4,KEY                                                           
         USING PRGRECD,R4                                                       
*                                                                               
         MVC   KEY(2),=X'0D81'                                                  
         MVC   PRGKAGMD,AGYMED                                                  
         MVC   PRGKCLT,CLIPK       CLIENT                                       
         MVC   PRGPID(3),PGRP      GROUP ID AND NUMBER                          
         MVC   PRGPPRD,0(R5)       PRODUCT CODE                                 
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(13),KEYSAVE     IS PRODUCT IN THIS GROUP?                    
         BE    OKREC               YES - KEEP IT                                
*                                                                               
         AHI   R5,3                NO - CHECK NEXT PRODUCT IN UNIT              
         B     FILREC21                                                         
*                                                                               
FILREC30 DS    0H                                                               
         B     OKREC                                                            
*                                                                               
         SPACE                                                                  
SKIPREC  LTR   RB,RB               SET CC TO NEQ                                
         B     *+6                                                              
OKREC    CR    RB,RB               SET CC TO EQ                                 
         B     EXXMOD                                                           
         SPACE                                                                  
FILBRAN  BC    0,FILREC5                                                        
FILBRAN2 BC    0,FILREC8                                                        
FILBRAN3 BC    0,SKIPREC                                                        
         DROP  R5                                                               
         EJECT                                                                  
* DISFIL - DISPLAY USER FILTERS                                                 
*                                                                               
* ON EXIT, FLD CONTAINS SPACE FILLED OUTPUT AND FLDH+7 CONTAINS OUTPUT          
*          LENGTH                                                               
*                                                                               
DISFIL   MVC   FLD,SPACES                                                       
         MVI   FLDH+7,1            INITIALIZE LEN TO 1                          
         LA    R2,FLD                                                           
         TM    FILTER,X'40'        TEST FOR NEGATIVE FILTER                     
         BO    DISFIL2             NO                                           
         MVI   0(R2),STAR                                                       
         MVI   FLDH+7,2                                                         
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
DISFIL2  MVC   0(1,R2),FILTER                                                   
         B     EXXMOD                                                           
         EJECT                                                                  
* GETRSN - DISPLAY REASON WHY UNIT IS NOT PAYABLE                               
*                                                                               
* ON ENTRY, P1 = A(PAYBLOCK)                                                    
*           P2 = A(OUTPUT AREA)                                                 
*                                                                               
GETRSN   LM    R2,R3,0(R1)                                                      
         USING PAYBLKD,R2                                                       
         ZIC   R1,PAYRSN                                                        
         MH    R1,=Y(L'RSNTAB)                                                  
         LA    R4,RSNTAB-L'RSNTAB(R1) INDEX TO REASON                           
         MVC   0(L'RSNTAB,R3),0(R4) EXTRACT REASON TEXT                         
         B     EXXMOD                                                           
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
* BUILD TABLE OF UNIT PRODUCTS IN WORK                                          
****************************************************************                
BLDPRLST NTR1                                                                   
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         MVI   0(R2),X'FF'                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   BPR10               YES                                          
         L     R6,12(R1)                                                        
         USING NUPDED,R6                                                        
*                                                                               
         SR    R4,R4                                                            
         ZIC   R5,NUPDELEN         GET # OF PRODUCTS                            
         SHI   R5,3                                                             
         D     R4,=F'6'                                                         
*                                                                               
         LA    R1,NUPDEPR          PRODUCT CODE                                 
*                                                                               
BPR05    DS    0H                                                               
         MVC   0(3,R2),0(R1)       PRODUCT CODE                                 
         AHI   R2,3                                                             
         MVI   0(R2),X'FF'                                                      
*                                                                               
         LA    R1,7(R1)            BUMP TO NEXT PRODUCT                         
         BCT   R5,BPR05                                                         
         B     BLDPRX                                                           
         DROP  R6                                                               
*                                                                               
BPR10    DS    0H                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
         CLI   NUPRD,0                                                          
         BE    BPR50               CHECK X'14' ELEMENTS                         
*                                                                               
         MVC   BYTE,NUPRD                                                       
         BAS   RE,GETPRDA          GET 3 CHAR PRODUCT CODE                      
*                                                                               
         MVC   0(3,R2),THREE       PRODUCT EQUATE                               
         AHI   R2,3                                                             
         MVI   0(R2),X'FF'                                                      
*                                                                               
         CLI   NUPRD2,0            ANY 2ND PRODUCT?                             
         BE    BLDPRX                                                           
*                                                                               
         MVC   BYTE,NUPRD2                                                      
         BAS   RE,GETPRDA          GET 3 CHAR PRODUCT CODE                      
*                                                                               
         MVC   0(3,R2),THREE       PRODUCT EQUATE                               
         AHI   R2,3                                                             
         MVI   0(R2),X'FF'                                                      
         B     BLDPRX                                                           
         DROP  R6                                                               
*                                                                               
BPR50    DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   BLDPRX              YES                                          
         L     R6,12(R1)                                                        
         USING NUPRDD,R6                                                        
*                                                                               
         SR    R4,R4                                                            
         ZIC   R5,NUPRDLEN         GET # OF PRODUCTS                            
         SHI   R5,3                                                             
         D     R4,=F'6'                                                         
*                                                                               
         LA    R1,NUPRDPR          PRODUCT CODE                                 
*                                                                               
BPR55    DS    0H                                                               
         MVC   BYTE,0(R1)                                                       
         BAS   RE,GETPRDA          GET 3 CHAR PRODUCT CODE                      
*                                                                               
         MVC   0(3,R2),THREE       PRODUCT EQUATE                               
         AHI   R2,3                                                             
         MVI   0(R2),X'FF'                                                      
*                                                                               
         LA    R1,6(R1)            BUMP TO NEXT PRODUCT                         
         BCT   R5,BPR55                                                         
         B     BLDPRX                                                           
         DROP  R6                                                               
*                                                                               
BLDPRX   DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
* GET PRODUCT ALPHA - 3 CHAR CODE IN "THREE"                                    
***********************************************************************         
GETPRDA  NTR1                                                                   
         L     R6,ACLIREC                                                       
         USING CLTHDRD,R6                                                       
*                                                                               
         XC    THREE,THREE                                                      
*                                                                               
         LA    R6,CLIST                                                         
         LA    R5,220              MAX # OF PRODUCTS                            
*                                                                               
GPRDA20  DS    0H                                                               
         OC    0(4,R6),0(R6)                                                    
         BZ    GPRDA25             NOT IN CLIST, CHECK CLIST2                   
*                                                                               
         CLC   3(1,R6),BYTE        SAME PRODUCT EQUATE?                         
         BE    GPRDA30                                                          
*                                                                               
         LA    R6,4(R6)                                                         
         BCT   R5,GPRDA20                                                       
*                                                                               
GPRDA25  DS    0H                                                               
         L     R6,ACLIREC                                                       
         LA    R6,CLIST2                                                        
         LA    R5,35               MAX # OF PRODUCTS                            
*                                                                               
GPRDA27  DS    0H                                                               
         OC    0(4,R6),0(R6)                                                    
         BZ    GETPRDAX            NOT IN CLIST2 EITHER                         
*                                                                               
         CLC   3(1,R6),BYTE                                                     
         BE    GPRDA30                                                          
*                                                                               
         LA    R6,4(R6)                                                         
         BCT   R5,GPRDA27                                                       
         DC    H'00'               HAS TO BE THERE OR ELSE DIE                  
         DROP  R6                                                               
*                                                                               
GPRDA30  DS    0H                                                               
         MVC   THREE,0(R6)         SAVE ALPHA PRODUCT                           
         OC    THREE,SPACES                                                     
*                                                                               
GETPRDAX DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
CRCKERR  DC    C'CR/CK MUST BE CLEARED SEPARATELY'                              
****************************************************************                
* CHKPROD - CHECK 3 CHAR PRODUCT                                                
****************************************************************                
CHKPROD  NTR1                                                                   
         L     RF,NBACLI                                                        
         CLI   1(RF),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   NBPR1CL3,X'40'      ARE WE USING 3 CHAR PROD?                    
         BNH   FILPRD1                                                          
         CLC   NBPR1CL3,PROD       YES - USE 3 CHAR PROD                        
         BE    ENDCHK                                                           
         BNE   FILPRD2                                                          
FILPRD1  CLC   PRDN,NBPRD          CHECK FIRST PROD AGAINST REC.                
         BE    ENDCHK                                                           
FILPRD2  DS    0H                  CHECK AGAINST PRODUCT ELELMENT               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',NBAIO),0,0                     
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   FILPRD4             NO                                           
         L     R5,12(R1)           YES                                          
         USING NUPDEEL,R5                                                       
         CLC   PROD,NUPDEPR        MATCH ON FIRST PROD                          
         BE    ENDCHK              SETS CONDITION CODE ON EXIT                  
         BNE   ENDCHK                                                           
         DROP  R5                                                               
FILPRD4  CLC   PRDN,NBPRDLST       CHECK AGAINST PRODUCT ELELMENT               
         BNE   ENDCHK              SETS CONDITION CODE ON EXIT                  
         B     ENDCHK                                                           
*                                                                               
ENDCHK   DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
****************************************************************                
* GETSPRAT - CHECKS SPECIAL REP CODES ON SPECIAL RATE ELEMENTS                  
****************************************************************                
GETSPRAT NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   GTSREX              YES                                          
         L     R5,12(R1)                                                        
         B     GTSR100                                                          
         USING NUSPRD,R5                                                        
GTSR050  ZIC   RE,NUSPRLEN                                                      
         AR    R5,RE                                                            
         CLI   NUSPREL,X'03'                                                    
         BE    GTSR100                                                          
         MVI   12(R1),X'FF'                                                     
         B     GTSREX                                                           
*                                                                               
GTSR100  TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    GTSR120             YES                                          
         CLC   SRATREP,NUSPRREP    TEST FOR MATCH ON SPECIAL REP                
         BE    GTSREX              YES                                          
         B     GTSR050             YES                                          
*                                                                               
GTSR120  OC    NUSPRREP,NUSPRREP   TEST FOR ANY SPECIAL REP                     
         BNZ   GTSREX                                                           
         B     GTSR050             NO-SKIP RECORD                               
*                                                                               
GTSREX   B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FMAX  = MAXIMUM SCAN LENGTH (OPTIONALLY SET BY USER)                   
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
FVAL     NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,ASPOOLA          ADDRESSABILITY TO SPOOL AREA                 
         XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FLEN,0              TEST IF RE-EDITING                           
         BE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         CLI   FMAX,0              TEST FOR USER SCAN LIMIT                     
         BE    *+8                 NO                                           
         IC    R3,FMAX             YES-USE IT IN PLACE OF DERIVED LIMIT         
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         BNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R0,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
         LR    R3,R0               SET REAL DATA LENGTH                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
FVAL12   CLI   0(RE),C'A'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'I'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'J'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'R'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'S'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'Z'                                                       
         BNH   FVAL14              VALID ALPHA                                  
*                                                                               
FVAL13   NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
*                                                                               
FVAL14   CLI   0(RE),C'0'          TEST IF NUMERIC                              
         BL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL12                                                        
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         CLI   FLDH+5,15                                                        
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FVALX                                                            
         PACK  DUB,FLD(0)          EXECUTED                                     
*                                                                               
FVALX    MVI   FMAX,0              ALWAYS CLEARED BY FVAL                       
         B     EXXMOD                                                           
         EJECT                                                                  
* I/O CONTROLLER                                                                
*     ON ENTRY   KEY      = CONTAINS KEY FOR READ                               
*                NDXDA    = CONTAINS DISK ADDRESS FOR D/A FILE READ             
*                                                                               
*     PARAMETERS                                                                
*                P1       = I/O MASK - COMMAND/FILE/DIRECTORY OR                
*                           FILE/CONTROL SETTINGS                               
*                P2       = BYTES 1-3 - A(I/O AREA) FOR FILE READ               
*                                                                               
*     AFTER I/O                                                                 
*                IOFLAG   = 0 OR ERROR SETTING                                  
*                KEYSAVE  = KEY PASSED TO ROUTINE                               
*                KEY      = CONTAINS KEY RETURNED BY DATAMGR                    
*                AIOAREA  = A(I/O AREA) FOR RECORD                              
*                                                                               
IO       NTR1  BASE=BASE1,WORK=(RC,IOWORKX-IOWORKD)                             
         L     RA,BASE2            SECOND BASE REGISTER                         
         USING IOWORKD,RC          LOCAL STORAGE                                
         LM    R2,R3,0(R1)         I/O MASK AND I/O AREA                        
         STC   R2,IOWORK1          LOW ORDER BYTE OF I/O MASK                   
         MVC   IOWORK2,IOWORK1     SAVE LOW ORDER BYTE                          
         STCM  R2,2,IOWORK3        SAVE THIRD BYTE                              
         NI    IOWORK2,X'0F'       ISOLATE COMMAND NUMBER                       
         ZIC   R1,IOWORK2                                                       
         SLL   R1,3                MULTIPLY BY 8 TO DEVELOP INDEX               
         LA    RE,CMNDTAB-L'CMNDTAB(R1) INDEX INTO COMMAND TABLE                
         MVC   IOCMND,0(RE)        EXTRACT COMMAND NAME                         
         SPACE 1                                                                
IO2      MVC   IOWORK2,IOWORK1     REFRESH MASK VALUES                          
         NI    IOWORK2,X'70'       ISOLATE FILE NUMBER                          
         ZIC   R1,IOWORK2          FILE NUMBER                                  
         SRL   R1,4                DIVIDE BY 16 TO DEVELOP INDEX                
         LA    R0,L'FILTAB                                                      
         MR    R0,R0                                                            
         LA    RE,FILTAB-L'FILTAB(R1)                                           
         MVC   IODIR,0(RE)         EXTRACT DIRECTORY NAME                       
         MVC   IOFILE,7(RE)        FILE NAME                                    
         MVC   IOVALS,14(RE)                                                    
         ZIC   R1,IOWORK1          TEST IOFLAG VS. EXCEPTION VALS               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    IOEXCPT,0                                                        
         BZ    *+6                                                              
         DC    H'0'                YES-BLOW UP - CHECK IOFLAG                   
         TM    IOWORK3,X'01'       TEST FOR PASS DELETES                        
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'08'                                                   
         TM    IOWORK3,X'02'       TEST FOR READ FOR UPDATE                     
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'80'                                                   
         TM    IOWORK1,DIR         TEST FOR DIRECTORY READ                      
         BZ    IO6                                                              
         SPACE 1                                                                
IO4      MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATMGR,IOPARM,(DMINBITS,IOCMND),IODIR,KEY,KEY                   
         LA    RE,KEY                                                           
         ZIC   R0,IODADSP                                                       
         AR    RE,R0               POINT TO DISK ADDRESS                        
         MVC   NDXDA,0(RE)                                                      
         MVC   IOFLAG,8(R1)                                                     
         B     IOX                                                              
         SPACE 1                                                                
IO6      ST    R3,AIOAREA                                                       
         LA    R2,NDXDA            POINT AT DISK ADDRESS                        
         TM    IOEXCPT,DIR         TEST FOR FILE W DIRECTORY                    
         BZ    IO7                                                              
         LA    R2,KEY              FOR IS FILE, POINT TO KEY                    
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
IO7      GOTO1 VDATMGR,IOPARM,(DMINBITS,IOCMND),IOFILE,(R2),AIOAREA,   X        
               DMWORK                                                           
         MVC   IOFLAG,8(R1)                                                     
         TM    IOEXCPT,DIR         TEST FOR IS FILE                             
         BZ    IOX                 NO                                           
         L     RF,AIOAREA                                                       
         ZIC   R1,IOKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IOX                                                              
         MVC   KEY(0),0(RF)        EXTRACT KEY FROM RECORD                      
         SPACE 1                                                                
IOX      MVI   DMINBITS,0                                                       
         TM    IOFLAG,X'FD'        TEST FOR DATAMGR ERROR                       
         BZ    IOXX                NONE                                         
         MVI   FERN,0                                                           
         TM    IOFLAG,X'E0'        TEST FOR DUPLICATE KEY ON ADD                
*                                  NON-RECOVERABLE DISK ERROR                   
*                                  END OF FILE                                  
         BZ    ERROR               NO-LET GETMSG SORT IT OUT                    
         DC    H'0'                YES-DUMP TO INSURE PROPER RECOVERY           
*                                                                               
IOXX     MVI   IOFLAG,0                                                         
         B     EXXMOD                                                           
         DROP  RC                                                               
         EJECT                                                                  
* RETURN FROM MODULE AND ROUTINES                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
* TABLE OF CORE-RESIDENT MODULE ADDRESSES                                       
*                                                                               
CORETAB  DS    0X                                                               
         DC    X'01020C0D0F101114152728'                                        
CORES    EQU   (*-CORETAB)                                                      
         SPACE 2                                                                
* TABLE OF BASE PROVIDED MODULES AND FACILITIES                                 
*                                                                               
BASETAB  DS    0F                                                               
         DC    V(GETBROAD)                                                      
         DC    V(GETPAY)                                                        
         DC    A(IO)                                                            
         DC    A(FVAL)                                                          
         DC    V(NETNET)                                                        
         DC    V(MSPACK)                                                        
         DC    V(NETBLRDR)                                                      
         DC    2A(0)                                                            
BASETABC EQU   (*-BASETAB)/L'BASETAB                                            
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS INTO WORKING STORAGE FOR EXTENDED ADCONS               
*                                                                               
ADTAB    DS    0AL3                                                             
         DC    AL3(ESTTAB-PAYWRKD)                                              
         DC    AL3(IOAREA1-PAYWRKD)                                             
         DC    AL3(IOAREA2-PAYWRKD)                                             
         DC    AL3(IOAREA3-PAYWRKD)                                             
         DC    AL3(IOAREA4-PAYWRKD)                                             
         DC    AL3(SPOOLA-PAYWRKD)                                              
         DC    AL3(CLIREC-PAYWRKD)                                              
         DC    AL3(OVWORK-PAYWRKD)                                              
EXADCONS EQU   (*-ADTAB)/L'ADTAB                                                
         SPACE 2                                                                
* TABLE OF BILLING STATUS VALUES                                                
*                                                                               
BILLTAB  DS    0CL6                                                             
         DC    CL4'YES',AL2(BILLED)                                             
         DC    CL4'NO',AL2(UNBILLED)                                            
         DC    CL4'TIME',AL2(TBILL)                                             
         DC    CL4'INT',AL2(INTBILL)                                            
         DC    CL4'CS',AL2(COPYBILL)                                            
         DC    CL4'BO',AL2(BLAKBILL)                                            
         DC    CL4'CI',AL2(CUTIBILL)                                            
         DC    CL4'OT',AL2(OTHBILL)                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF CLEARED STATUS VALUES                                                
*                                                                               
CLEARTAB DS    0CL6                                                             
         DC    CL4'YES',AL2(PAID)                                               
         DC    CL4'NO',AL2(UNPAID)                                              
         DC    CL4'PART',AL2(PARTPAID)                                          
         DC    CL4'TIME',AL2(TPAID)                                             
         DC    CL4'INT',AL2(INTPAID)                                            
         DC    CL4'CS',AL2(COPYPAID)                                            
         DC    CL4'BO',AL2(BLAKPAID)                                            
         DC    CL4'CI',AL2(CUTIPAID)                                            
         DC    CL4'OT',AL2(OTHPAID)                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF REASONS FOR UNPAYABLE UNITS (MATCHES EQUATE NUMBERS)                 
*                                                                               
RSNTAB   DS    0CL16                                                            
         DC    CL16'*UNALLOCATED*'                                              
         DC    CL16'*NO ACTUAL COST*'                                           
         DC    CL16'*TIME NOT PAID*'                                            
REASONS  EQU   (*-RSNTAB)/L'RSNTAB                                              
         SPACE 2                                                                
* TABLE OF I/O COMMANDS                                                         
*                                                                               
CMNDTAB  DS    0CL8                                                             
         DC    CL8'DMREAD'                                                      
         DC    CL8'DMRSEQ'                                                      
         DC    CL8'DMRDHI'                                                      
         DC    CL8'DMWRT'                                                       
         DC    CL8'DMADD'                                                       
         DC    CL8'GETREC'                                                      
         DC    CL8'PUTREC'                                                      
         DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
* TABLE OF FILES/DIRECTORIES AND THEIR ATTRIBUTES                               
*                                                                               
FILTAB   DS    0XL17                                                            
         DC    CL7'SPTDIR',CL7'SPTFILE',AL1(0,13,14)                            
         DC    CL7'UNTDIR',CL7'UNTFIL',AL1(0,20,21)                             
         DC    CL7'STATION',CL7'STATION',AL1(DIR,17,0)                          
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         EJECT                                                                  
* DSECT TO COVER I/O ROUTINE LOCAL STORAGE                                      
*                                                                               
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC, PUTREC, AND ADDREC               
IOPARM   DS    6F                  PARAMETER LIST                               
IOWORK1  DS    X                                                                
IOWORK2  DS    X                                                                
IOWORK3  DS    X                                                                
IOCMND   DS    CL7                 DATA MANAGER COMMAND                         
IODIR    DS    CL7                 DIRECTORY NAME                               
IOFILE   DS    CL7                 FILE NAME                                    
IOVALS   DS    0XL3                                                             
IOEXCPT  DS    X                   EXCEPTION IOFLAG VALUES FOR FILE             
IOKEYL   DS    X                   KEY LENGTH                                   
IODADSP  DS    X                   DISPLACEMENT TO DISK ADDRESS                 
IOWORKX  EQU   *                                                                
         EJECT                                                                  
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENPRD (PRDHDRD)                                                            
         PRINT OFF                                                              
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* FACPAK DSECTS                                                                 
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086NEPAY00   11/20/17'                                      
         END                                                                    
