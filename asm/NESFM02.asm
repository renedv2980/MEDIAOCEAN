*          DATA SET NESFM02    AT LEVEL 154 AS OF 04/06/20                      
*PHASE T31C02B,*                                                                
*INCLUDE BINSRCH2                                                               
***********************************************************************         
*                                                                               
*  TITLE: T31C02 - MAINTENANCE/LIST OF PRODUCT RECORDS                          
*                                                                               
*  COMMENTS: MAINTAINS PRODUCT RECORDS                                          
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C40), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMC3 (T31CC3) -- MAINTENANCE                              
*                  NESFMC4 (T31CC4) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW PRODUCTS                                             
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
*   HIST: (PLEASE INSERT BRIEF DESCRIPTION OF UPDATES)                          
*                        ******* READ THIS !!!!!! *******                       
*  06DEC/91  (SKU)  ---  WAITING FOR CHRIS O'CONNOR TO UPDATE TERMINAL          
*                        RECS. BEFORE WE CAN RUN THIS VERSION                   
*                   ---  PRODUCT DELETE UPDATES PRODUCT CODE LIST IN            
*                        CLIENT RECORD                                          
*                                                                               
*  20JAN/92  (SKU)  ---  NEW SECURITY ROUTINE TO CHECK CLIENT ACCESS.           
*                        TERMINAL RECS. ARE READY (SEE PREVIOUS)                
*                                                                               
*  14FEB/92  (SKU)  ---  LEFT ALIGN COMMISSION RATE                             
*                                                                               
*  24MAR/92  (SKU)  ---  FIX BUG TO DISPLAY BILL FORMULA                        
*                                                                               
*  25NOV/92  (SKU)  ---  FIX DELETED BUG                                        
*                                                                               
***********************************************************************         
         TITLE 'T31C02 NETWORK PRODUCT RECORD'                                  
T31C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C02,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31C02,RB,R7                                                     
         ST    R2,RELO                                                          
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         GOTO1 VTERMACC            CHECK TERMINAL ACCESS                        
*                                                                               
VALK     CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        ADD PASSIVE POINTER                          
         BE    XA                                                               
         CLI   MODE,RECDEL         CAN'T SPECIFY DELETE HERE                    
         BE    DELMSG                                                           
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         GOTO1 =A(LISTREC),DMCB,(1,DUB),(R9),(RA),(RC),RR=RELO                  
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   EXIT                                                             
         GOTO1 =A(LISTREC),DMCB,(1,DUB),(R9),(RA),(RC),RR=RELO                  
EXIT     XIT1                                                                   
*                                                                               
MAXPRD   EQU   252                 MAXIMUM NUMBER OF PRODUCTS!!                 
MAXPRDNP EQU   251                 MAXIMUM NUMBER OF NON POOL PRODUCTS          
TOTPRDS  EQU   500                                                              
         EJECT                                                                  
***********************************************************************         
*     VALIDATE KEY ROUTINE                                                      
***********************************************************************         
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PKEY,R6                                                          
         MVI   NOPTFLG,0                                                        
         SPACE                                                                  
         LA    R2,PRDMEDH              * MEDIA                                  
         GOTO1 VALIMED                                                          
         MVC   PKEYAM,BAGYMD                                                    
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST      IF IT'S LIST                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       IF IT'S REPORT                               
         BNE   VK50                                                             
*                                                                               
         MVI   LSTFLTS,0                                                        
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,LPDFILH                                                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0                                                         
         BE    TRAPERR                                                          
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
*                                                                               
VK05     DS    0H                                                               
         CLI   FLD1LEN,0                                                        
         BE    VK20                                                             
*                                                                               
         CLC   FLD1(4),=C'THNT'                                                 
         BNE   *+12                                                             
         OI    LSTFLTS,FLTTHNT                                                  
         B     VK10                                                             
*                                                                               
         CLC   FLD1(4),=C'THTR'                                                 
         BNE   TRAPERR                                                          
         OI    LSTFLTS,FLTTHTR                                                  
         DROP  R5                                                               
*                                                                               
VK10     DS    0H                                                               
         LA    R5,32(R5)                                                        
         B     VK05                                                             
*                                                                               
VK20     DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,LPDCLTH                                                       
         GOTO1 VALIFLD                                                          
         BZ    TRAPERR                                                          
         GOTO1 VALICLT                                                          
         MVC   PKEYCLT,BCLT                                                     
         B     VKX                                                              
*                                                                               
VK50     LA    R2,PRDCLTH             * CLIENT                                  
         GOTO1 VALIFLD                                                          
         BZ    VK60                                                             
         GOTO1 VALICLT                                                          
         MVC   PKEYCLT,BCLT                                                     
*                                                                               
         XC    USERFLD1,USERFLD1                                                
         XC    USERFLD2,USERFLD2                                                
         CLI   PRDUSR1H+5,0        IS THERE SOMETHING IN HERE ALREADY?          
         BNE   *+10                                                             
         XC    PRDUSR1,PRDUSR1                                                  
         XC    PRDDSC1,PRDDSC1                                                  
         OI    PRDUSR1H+1,X'20'    SET PROTECTED                                
         CLC   SVP1USER,SPACES                                                  
         BNH   VK55                                                             
         MVC   PRDDSC1,SVP1USER                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   USERFLD1,PUSER1                                                  
         NI    PRDUSR1H+1,X'FF'-X'20'                                           
*                                                                               
VK55     CLI   PRDUSR2H+5,0        IS THERE SOMETHING IN HERE ALREADY?          
         BNE   *+10                                                             
         XC    PRDUSR2,PRDUSR2                                                  
         XC    PRDDSC2,PRDDSC2                                                  
         OI    PRDUSR2H+1,X'20'    SET PROTECTED                                
         CLC   SVP2USER,SPACES                                                  
         BNH   VK56                                                             
         MVC   PRDDSC2,SVP2USER                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   USERFLD2,PUSER2                                                  
         NI    PRDUSR2H+1,X'FF'-X'20'                                           
*                                                                               
VK56     OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
         OI    PRDUSR1H+6,X'80'                                                 
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
VK60     LA    R2,PRDPRDH                                                       
         GOTO1 VALIFLD                                                          
         BZ    VKX                                                              
         CLI   ACTNUM,ACTADD       IF ITS ADD                                   
         BNE   VK70                                                             
         MVI   ERROR,INVALID                                                    
         CLC   8(3,R2),=CL3'UNA'                                                
         BE    TRAPERR                                                          
         CLC   8(3,R2),=CL3'ALL'                                                
         BE    TRAPERR                                                          
         CLC   8(3,R2),=CL3'ZZZ'                                                
         BE    TRAPERR                                                          
         CLC   8(3,R2),=CL3'YES'                                                
         BE    TRAPERR                                                          
         CLC   8(3,R2),=CL3'NO '                                                
         BE    TRAPERR                                                          
         CLC   8(3,R2),=XL3'D5D600'                                             
         BE    TRAPERR                                                          
         CLI   5(R2),2             CHECK IT HERE                                
         BL    TRAPERR                                                          
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   NFLD,C'A'           FIRST POSITION MUST BE ALPHA                 
         BL    TRAPERR                                                          
         CLI   NFLD,C'Z'                                                        
         BH    TRAPERR                                                          
         BAS   RE,VALALPH          TEST PRODUCT IS ALPHANUMERIC                 
         MVC   PKEYPRD,NFLD                                                     
         MVC   QPRD,NFLD           ALSO SET QPRD                                
         B     VKX                                                              
VK70     DS    0H                                                               
         CLC   =C'AAA',8(R2)       FOR SOME REASON VALIPRD DOES NOT             
         BNE   VK80                LIKE =C'AAA' IN THE CONTROLLER?????          
         MVC   PKEYPRD,8(R2)                                                    
         B     VKX                                                              
VK80     GOTO1 VALIPRD                                                          
         MVC   PKEYPRD,QPRD                                                     
****     MVC   PRDUSR1,USERFLD1                                                 
****     MVC   PRDUSR2,USERFLD2                                                 
         MVC   PRDUSR1,USERFLD1                                                 
         MVC   PRDUSR2,USERFLD2                                                 
         SPACE                                                                  
VKX      DS    0H                                                               
         LA    R2,PRDMEDH                                                       
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
VALALPH  NTR1                      TEST PRODUCT CODE IS ALPHANUMERIC            
         LA    RF,ALPHANUM                                                      
VALPH10  CLI   0(RF),X'FF'                                                      
         JE    TRAPERR                                                          
         CLC   NFLD(1),0(RF)                                                    
         JE    *+12                                                             
         AHI   RF,1                                                             
         J     VALPH10                                                          
*                                                                               
         LA    RF,ALPHANUM                                                      
VALPH20  CLI   0(RF),X'FF'                                                      
         JE    TRAPERR                                                          
         CLC   NFLD+1(1),0(RF)                                                  
         JE    *+12                                                             
         AHI   RF,1                                                             
         J     VALPH20                                                          
*                                                                               
         CLI   NFLD+2,C' '                                                      
         JE    EXIT                                                             
         CLI   NFLD+2,0                                                         
         JE    EXIT                                                             
         LA    RF,ALPHANUM                                                      
VALPH30  CLI   0(RF),X'FF'                                                      
         JE    TRAPERR                                                          
         CLC   NFLD+2(1),0(RF)                                                  
         JE    EXIT                                                             
         AHI   RF,1                                                             
         J     VALPH30                                                          
*                                                                               
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         BAS   RE,CLT0J            CONVERT 0/J CLIENT CODE                      
*                                                                               
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
         MVI   PRDMED,C'N'                   * MEDIA                            
         OI    PRDMEDH+6,X'80'     XMIT                                         
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,(WORK2,PKEYCLT),PRDCLT    * CLIENT                   
         OI    PRDCLTH+6,X'80'     XMIT                                         
         MVI   PRDCLTH+5,2                                                      
         CLI   PRDCLT+2,C' '                                                    
         JE    *+8                                                              
         MVI   PRDCLTH+5,3                                                      
         SPACE                                                                  
         CLI   PKEYPRD,X'FF'                 * PRODUCT                          
         BNE   *+14                                                             
         MVC   PRDPRD(3),=C'POL'                                                
         B     *+10                                                             
         MVC   PRDPRD,PKEYPRD                                                   
         OI    PRDPRDH+6,X'80'     XMIT                                         
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   BYTE,C'A'                                                        
         LA    R2,PRDCLTH             * CLIENT                                  
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR010                                                            
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR010    LA    R2,PRDNAMEH                 CLEAR SCREEN                         
         BAS   RE,CLRSCRN                                                       
         XC    PRDLDAT,PRDLDAT                                                  
         OI    PRDLDATH+6,X'80'    XMIT                                         
*                                                                               
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
*                                           PRODUCT NAME                        
         MVC   PRDNAME,PNAME                                                    
         OI    PRDNAMEH+6,X'80'    XMIT                                         
*                                           PRODUCT/CLIENT CODE                 
         CLI   PACCT,X'FF'                                                      
         BNE   DR015                                                            
         UNPK  PRDACCT(5),PACCT+1(3)                                            
         OI    PRDACCTH+6,X'80'    XMIT                                         
         B     DR024                                                            
*                                                                               
DR015    MVC   PRDACCT(4),PACCT                                                 
         OI    PRDACCTH+6,X'80'    XMIT                                         
*                                           PRODUCT CLASS                       
         LA    R2,PRDCLASH                                                      
         CLI   PCLASS,0                                                         
         BE    DR024                                                            
         MVC   FULL(1),PCLASS                                                   
         MVI   FULL+1,C' '                                                      
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    DR020               NO                                           
         PACK  FULL(1),PCLASS      FIRST CLASS/SWAPS NIBBLES IN FULL            
         NI    FULL,X'0F'                                                       
         OI    FULL,X'C0'          MAKE A - I                                   
         MVC   FULL+1(1),PCLASS    2ND CLASS                                    
         NI    FULL+1,X'0F'                                                     
         OI    FULL+1,X'C0'        MAKE A - I                                   
DR020    MVC   PRDCLAS(2),FULL                                                  
         OI    PRDCLASH+6,X'80'    XMIT                                         
*                                                                               
DR024    MVC   PRDLOCK,PLOCK                                                    
         OI    PRDLOCK,X'40'                                                    
         OI    PRDLOCKH+6,X'80'    XMIT                                         
****     GOTO1 DATCON,DMCB,(2,PLKDAT),(8,PRDLDAT)                               
****     OI    PRDLDATH+6,X'80'    PROD LOCK ACTV DATE                          
*                                           PRODUCT ADDRESS 1                   
DR030    MVC   PRDBNAM(30),PADDR1                                               
         OI    PRDBNAMH+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 2                   
         MVC   PRDADD2(30),PADDR2                                               
         OI    PRDADD2H+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 3                   
         MVC   PRDADD3(30),PADDR3                                               
         OI    PRDADD3H+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 4                   
         MVC   PRDADD4(30),PADDR4                                               
         OI    PRDADD4H+6,X'80'    XMIT                                         
*                                           PRODUCT AGENCY FEE                  
         LA    R2,PRDOAFH                                                       
         CLC   PAGYFEE(2),=6X'00'                                               
         BE    DR050                                                            
         EDIT  (P2,PAGYFEE),(5,PRDOAF),2,ALIGN=LEFT                             
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                            BILL BASIS                         
DR050    XC    PRDBBAS,PRDBBAS                                                  
         XC    PRDCPCT,PRDCPCT                                                  
         XC    PRDCBAS,PRDCBAS                                                  
         XC    PRDEDAT,PRDEDAT                                                  
         OI    PRDBBASH+6,X'80'    XMIT                                         
         OI    PRDCPCTH+6,X'80'    XMIT                                         
         OI    PRDCBASH+6,X'80'    XMIT                                         
         OI    PRDEDATH+6,X'80'    XMIT                                         
*                                                                               
         OC    PBILLBAS(5),PBILLBAS                                             
         BZ    DR095                                                            
*                                                                               
         LA    R2,PRDBBASH                                                      
         MVC   8(5,R2),=CL5'CNET'                                               
         TM    PBILLBAS,X'50'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=CL5'NET'                                                
         TM    PBILLBAS,X'10'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=C'CGROS'                                                
         TM    PBILLBAS,X'40'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=C'GROSS'                                                
*                                                                               
DR060    OI    6(R2),X'80'         XMIT                                         
*                                                                               
         L     R5,PBILLCOM                                                      
         LTR   R5,R5                                                            
         BZ    DR095                                                            
*                                             COMMISION PERCENT                 
         LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WONT FIT                         
         BNE   DR070                                                            
         MVC   PRDCPCT+1(3),=C'100'                                             
         B     DR075                                                            
DR070    EDIT  (R5),(8,PRDCPCT),4,FLOAT=+,ALIGN=LEFT                            
DR075    LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   PRDCPCT,C'-'        SET TO MINUS                                 
         OI    PRDCPCTH+6,X'80'    XMIT                                         
*                                  COMMISION BASIS                              
DR080    LA    R2,PRDCBASH                                                      
         MVC   8(5,R2),=C'GROSS'                                                
         TM    PBILLBAS,X'01'                                                   
         BZ    DR090                                                            
         MVC   8(5,R2),=C'NET  '                                                
DR090    OI    6(R2),X'80'         XMIT                                         
*                                  EFFECTIVE DATE                               
DR095    DS    0H                                                               
         CLC   PBILLDT,=6X'00'                                                  
         BE    DR100                                                            
         GOTO1 DATCON,DMCB,(3,PBILLDT),(9,PRDEDAT)                              
         LA    R2,PRDEDATH                                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR100    XC    PRDUSR1,PRDUSR1                                                  
         XC    PRDDSC1,PRDDSC1                                                  
         OI    PRDUSR1H+1,X'20'    SET PROTECTED                                
         CLC   SVP1USER,SPACES                                                  
         BNH   *+20                                                             
         MVC   PRDDSC1,SVP1USER                                                 
         MVC   PRDUSR1,PUSER1                                                   
         NI    PRDUSR1H+1,X'FF'-X'20'                                           
*                                                                               
         XC    PRDUSR2,PRDUSR2                                                  
         XC    PRDDSC2,PRDDSC2                                                  
         OI    PRDUSR2H+1,X'20'    SET PROTECTED                                
         CLC   SVP2USER,SPACES                                                  
         BNH   *+20                                                             
         MVC   PRDDSC2,SVP2USER                                                 
         MVC   PRDUSR2,PUSER2                                                   
         NI    PRDUSR2H+1,X'FF'-X'20'                                           
*                                                                               
         OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
         OI    PRDUSR1H+6,X'80'                                                 
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
*  OPTIONS                                                                      
*                                                                               
         XC    PRDOPTS,PRDOPTS                                                  
         LA    R2,PRDOPTSH                                                      
         LA    RF,8(R2)                                                         
*                                                                               
         TM    POPT1,POPT1_NOBILL                                               
         BZ    DR105                                                            
         MVC   0(7,RF),=C'BILL=NO'                                              
         AHI   RF,7                                                             
*                                                                               
DR105    DS    0H                                                               
         TM    POPT1,POPT1_RFC                                                  
         BZ    DR110                                                            
*                                                                               
         LA    RE,PRDOPTS          IS THERE AN OPTION BEFORE THIS?              
         CR    RF,RE                                                            
         BE    *+12                                                             
         MVI   0(RF),C','          YES, THEN FIRST INSERT COMMA                 
         AHI   RF,1                                                             
         MVC   0(5,RF),=C'RFC=Y'                                                
         AHI   RF,5                                                             
*                                                                               
DR110    DS    0H                                                               
         TM    POPT1,POPT1_THTR                                                 
         BZ    DR115                                                            
*                                                                               
         LA    RE,PRDOPTS          IS THERE AN OPTION BEFORE THIS?              
         CR    RF,RE                                                            
         BE    *+12                                                             
         MVI   0(RF),C','          YES, THEN FIRST INSERT COMMA                 
         AHI   RF,1                                                             
         MVC   0(4,RF),=C'THTR'                                                 
         LA    RF,4(RF)                                                         
*                                                                               
DR115    DS    0H                                                               
         TM    POPT1,POPT1_THNT                                                 
         BZ    DR120                                                            
*                                                                               
         LA    RE,PRDOPTS          IS THERE AN OPTION BEFORE THIS?              
         CR    RF,RE                                                            
         BE    *+12                                                             
         MVI   0(RF),C','          YES, THEN FIRST INSERT COMMA                 
         AHI   RF,1                                                             
         MVC   0(4,RF),=C'THNT'                                                 
         LA    RF,4(RF)                                                         
*                                                                               
DR120    DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
DELMSG   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ACTION DELETE INVALID - PLEASE RE-ENTER'          
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
* ADD PASSIVE POINTER                                                           
***********************************************************************         
XA       DS    0H             RECORD HAS BEEN ADDED/ADD PASSIVE POINTER         
         L     R6,AIO                                                           
         USING PRODD,R6                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    XA20                                                             
*                                                                               
         L     RF,ACOMFACS                 IF TEST SYSTEM ?                     
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FASYSID,1                                                        
         BE    EXIT                SKIP/SINCE THIS BRINGS DOWN TEST             
         DC    H'0'                                                             
         DROP  R1                                                               
                                                                                
XA20     MVC   WORK(4),KEY+14      SAVE DISKADDRESS                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF1'                                                  
         MVC   KEY+2(1),PKEYAM                                                  
         MVC   KEY+3(2),PKEYCLT                                                 
         CLC   PKEYPRD,=CL3'POL'                                                
         BNE   *+8                                                              
         MVI   KEY+5,X'FF'                                                      
         MVC   KEY+6(3),PKEYPRD                                                 
         MVC   KEY+9(2),PCODE                                                   
         MVC   KEY+14(4),WORK      INSERT DISK ADDRESS                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
XAX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       L     R6,AIO                                                           
         USING PKEY,R6                                                          
         MVI   OVPRDSW,0                                                        
         LA    R2,PRDNAMEH         PRODUCT NAME                                 
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   VR010                                                            
         CLI   T31CFFD+1,C'*'      DDS ONLY CAN DELETE                          
         BNE   TRAPERR                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    TRAPERR                                                          
*  ALLOW DELETE OF PRODUCT HEADER IF NO ESTIMATES EXIST FOR THAT PROD           
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),SAVEKEY                                                   
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL05                                                            
         CLI   KEY+7,0                                                          
         BNE   ESTERR                                                           
DEL05    DS    0H                  DELETE PRODUCT HEADER                        
         MVC   KEY,SAVEKEY                                                      
         MVI   KEY+13,X'DD'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
*  DELETE PASSIVE POINTER ODF1                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0DF1'                                                
         MVC   KEY+2(3),SAVEKEY+1                                               
         MVC   KEY+6(3),SAVEKEY+4                                               
         CLC   SAVEKEY+4(3),=CL3'POL'                                           
         BNE   *+8                                                              
         MVI   KEY+5,X'FF'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DELPRDCD,KEY+10                                                  
         MVI   KEY+13,X'DD'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
*  DELETE THE RECORD                                                            
         L     RE,AIO                                                           
         MVI   15(RE),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
*                                                                               
*  CLEAR PRODUCT CODE FROM CLIENT CODE LIST IN CLIENT RECORD                    
*                                                                               
*  CHECK IF DELETED PRODUCT ON OVERFLOW                                         
         CLI   DELPRDCD,0                                                       
         BNE   *+8                                                              
         BAS   RE,CHKOVSTA          CHECK FOR RESET OF CLIENT OV STATUS         
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         L     R1,AIO1                                                          
         MVC   KEY+1(3),1(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
*                                                                               
         MVC   QPRD,PRDPRD         PRODUCT CODE                                 
         OC    QPRD,SPACES         BLANK PAD                                    
*                                                                               
         L     R6,AIO2                                                          
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   DELPRDCD,0           WAS OVERFLOW PRODUCT DELETED                
         BNE   DEL10                                                            
         CLI   OVPRDSW,C'Y'         SHOULD OVFLW PROD SWITCH BE RESET           
         BE    *+8                                                              
         NI    CINDS1,X'FF'-CIN1OVP                                             
         B     DEL50                                                            
*                                                                               
DEL10    LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         LA    R4,CLIST2                                                        
         LA    R5,140                                                           
         LA    RE,SVCLIST2                                                      
         LA    RF,140                                                           
         MVCL  RE,R4                                                            
*                                                                               
DEL15    LA    RF,MAXPRD                                                        
         LA    R4,SVCLIST                                                       
DEL20    CLI   3(R4),0             END OF LIST?                                 
         BNE   *+6                                                              
         DC    H'0'                IF NOT FOUND, FATAL ERROR!!                  
         CLC   QPRD,0(R4)          MATCH ON PRODUCT?                            
         BE    DEL30               YES                                          
         LA    R4,4(R4)                                                         
         BCT   RF,DEL20                                                         
         DC    H'0'                                                             
*                                                                               
DEL25    CLI   3(R4),0             END OF LIST?                                 
         BE    DEL40                                                            
DEL30    MVC   0(4,R4),4(R4)       BUMP PRODUCT OUT OF CLIST AND                
         LA    R4,4(R4)            WRITE BACK REVISED CLIENT RECORD             
         BCT   RF,DEL25                                                         
*                                                                               
DEL40    LA    R4,SVCLIST          MOVE IN UPDATED LIST                         
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         LA    R5,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         LA    RE,CLIST2                                                        
         LA    R4,SVCLIST2                                                      
         LA    R5,140                                                           
         LA    RF,140                                                           
         CLI   SVCLIST2+3,0        MORE THEN 219 PRODUCTS?                      
         BNE   DEL45               YES                                          
         SR    R4,R4               NO, CLEAR CLIST2                             
         SR    R5,R5                                                            
DEL45    MVCL  RE,R4                                                            
         DROP  R6                                                               
*                                                                               
DEL50    GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         MVC   PRDNAME(19),=C'* PRODUCT DELETED *'                              
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
VR010    L     R6,AIO                                                           
         USING PRODD,R6                                                         
         MVC   PNAME,PRDNAME                                                    
         LA    R2,PRDACCTH         CLT/PRD CODE                                 
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
*                                                                               
         MVC   AGYALPHA,14(RA)                                                  
         CLC   =C'GY',AGYALPHA                                                  
         BE    VR030                                                            
*******  CLC   =C'DR',AGYALPHA                                                  
*******  BE    VR030                                                            
         CLC   =C'GN',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'CE',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'FM',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'RE',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'H9',AGYALPHA     FOR STARCOM MUST BE 3 OR GREATER             
         BNE   VR015                                                            
         XC    PACCT,PACCT                                                      
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         B     VR020                                                            
VR015    CLI   5(R2),4                                                          
         BNE   TRAPERR                                                          
         CLC   =C'BM ',PRDCLT       BRISTOL MYERS                               
         BNE   VR020                                                            
         CLC   =C'BO',AGYALPHA                                                  
         BNE   VR020                                                            
         TM    4(R2),X'08'         MUST BE 4 NUMERICS                           
         BZ    TRAPERR                                                          
*                                                                               
VR020    ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PACCT(0),8(R2)                                                   
         B     VR040                                                            
VR030    CLI   5(R2),5         FM,GY,DR,GN,CE MUST BE 5 NUMERICS                
         BNE   TRAPERR                                                          
         TM    4(R2),X'08'                                                      
         BZ    TRAPERR                                                          
         PACK  PACCT(4),8(5,R2)                                                 
         MVI   PACCT,X'FF'                                                      
         B     VR040                                                            
*                                                                               
VR040    DS    0H                                                               
         LA    R2,PRDCLASH         PROD CLASS(SINGLE OR DOUBLE)                 
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    VR050               NO CLASS                                     
         MVC   FULL(2),8(R2)                                                    
         MVC   BYTE,FULL                                                        
         CLI   FULL+1,C' '         IS 2ND CLASS GIVEN                           
         BNH   VR050               NO                                           
*                                                                               
         CLI   FULL,C'A'           BOTH MUST BE A-I                             
         BL    VR060                                                            
         CLI   FULL,C'I'                                                        
         BH    VR060                                                            
         CLI   FULL+1,C'A'                                                      
         BL    VR060                                                            
         CLI   FULL+1,C'I'                                                      
         BH    VR060                                                            
*                                                                               
         CLC   FULL(1),FULL+1      AND NOT THE SAME                             
         BE    VR060                                                            
*                                  HOLD AS 2 NIBBLES 1-9                        
         NI    FULL,X'0F'                                                       
         NI    FULL+1,X'0F'                                                     
         PACK  BYTE,FULL(1)        1ST CLASS                                    
         OC    BYTE,FULL+1         2ND CLASS                                    
*                                                                               
VR050    MVC   PCLASS,BYTE                                                      
*                                                                               
         LA    R2,PRDLOCKH         PRODUCT LOCKOUT                              
         CLI   5(R2),0                                                          
         BE    VR070                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VR053                                                            
         CLI   8(R2),C'N'                                                       
         BNE   VR060                                                            
VR053    MVC   PLOCK,8(R2)                                                      
*******  GOTO1 DATCON,DMCB,(5,0),(2,PLKDAT)     PROD LOCK ACTV DATE             
         B     VR070                                                            
*                                                                               
VR060    MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
VR070    LA    R2,PRDBNAMH                                                      
         GOTO1 ANY                                                              
         XC    PADDR1(120),PADDR1                                               
         MVC   PADDR1,PRDBNAM                                                   
         OC    PADDR1,SPACES                                                    
         LA    R2,PRDADD2H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR2,8(R2)                                                     
         OC    PADDR2,SPACES                                                    
         LA    R2,PRDADD3H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR3,8(R2)                                                     
         OC    PADDR3,SPACES                                                    
         LA    R2,PRDADD4H                                                      
         CLI   5(R2),0                                                          
         BE    VR080                                                            
         MVC   PADDR4,8(R2)                                                     
         OC    PADDR4,SPACES                                                    
*                                                                               
*                                                                               
VR080    LA    R2,PRDOAFH                                                       
         CLI   5(R2),0                                                          
         BNE   VR090                                                            
         XC    PAGYFEE,PAGYFEE                                                  
         B     VR100                                                            
*                                                                               
VR090    SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,PRDOAF),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     TRAPERR                                                          
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'999'        CAN'T EXCEED 9.99                             
         BH    TRAPERR                                                          
         ZAP   PAGYFEE,DUB                                                      
VR100    MVI   ERROR,INVALID                                                    
VR110    LA    R2,PRDBBASH                                                      
         CLI   5(R2),0                                                          
         BNE   VR120                                                            
         XC    PBILLBAS(5),PBILLBAS                                             
         B     VR140                                                            
         EJECT                                                                  
*                                                                               
VR120    MVI   ERROR,INVALID                                                    
         XC    PBILLBAS(5),PBILLBAS                                             
         GOTO1 ANY                      R2 $S AT ESTBBASH                       
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   VR130                                                            
         OI    PBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1                                                          
         BE    TRAPERR             NO 'C' ALONE                                 
         LA    R5,1(R5)            BUMP PAST 'C'                                
VR130    EX    R4,GROSCOM                                                       
         BE    VR140                                                            
         EX    R4,NETCOM                                                        
         BNE   TRAPERR                                                          
*                                       CHECK PROFILE                           
         OI    PBILLBAS,X'10'                                                   
         B     VR140                                                            
*                                                                               
GROSCOM  CLC   0(0,R5),=C'GROSS'        ALLOW G-GROSS                           
NETCOM   CLC   0(0,R5),=C'NET  '        OR N-NET                                
*                                                                               
VR140    LA    R2,PRDCPCTH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR160                                                            
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,PRDCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     TRAPERR                                                          
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'      100.0000 MAX                                 
         BH    TRAPERR                                                          
         C     R0,=F'0'                                                         
         BNH   TRAPERR                                                          
         CLI   PRDCPCT,C'-'                                                     
         BNE   VR150                    ERROR                                   
         LCR   R0,R0                    MAKE NEGATIVE                           
VR150    ST    R0,FULL                                                          
         MVC   PBILLCOM,FULL                                                    
         B     VR170                                                            
VR160    MVI   ERROR,INVALID            PRIOR (MSSNGERR)                        
         CLI   PRDCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   TRAPERR                                                          
         B     VR170                                                            
         EJECT                                                                  
*                                                                               
VR170    LA    R2,PRDCBASH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR190                                                            
         MVI   ERROR,INVALID                                                    
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BE    VR180                                                            
         EX    R4,NETCOM                                                        
         BNE   TRAPERR                                                          
         OI    PBILLBAS,X'01'                                                   
VR180    B     VR200                                                            
*                                                                               
VR190    MVI   ERROR,INVALID            PRIOR (MSSNGERR)                        
         CLI   PRDCPCTH+5,0                                                     
         BNE   TRAPERR                                                          
*                                                                               
VR200    OC    PBILLBAS(5),PBILLBAS           WILL BE ZEROS IF                  
         BNZ   VR210                          GROSS ALONE                       
         CLI   PRDBBASH+5,0                                                     
         BE    VR230           NO FORMULA                                       
*                              MUST HAVE BEEN GROSS ALONE                       
*                              PUT X'80' SO BILLING WILL THINK IT'S             
*                              A FORMULA                                        
         B     VR220                                                            
VR210    OC    PBILLCOM,PBILLCOM                                                
         BNZ   VR230                                                            
         CLI   PBILLBAS,X'40'      WAS GROSS ALONE + COMMISSION ONLY            
         BNE   VR230                                                            
VR220    OI    PBILLBAS,X'80'                                                   
*                                                                               
VR230    LA    R2,PRDEDATH            EFFECTIVE DATE OF BILL FORMULA            
         OC    PBILLBAS(5),PBILLBAS           SEE IF FORMULA INPUT              
         BNZ   VR240                                                            
         XC    PBILLDT,PBILLDT                                                  
         CLI   5(R2),0                                                          
         BE    VR245                                                            
         MVI   ERROR,INVALID      NO FORMULA SO EFF DATE INVALID                
         B     TRAPERR                                                          
*                                                                               
VR240    GOTO1 ANY          REQUIRED IF FORMULA INPUT                           
         GOTO1 DATVAL,DMCB,(2,PRDEDAT),WORK                                     
         MVI   ERROR,INVALID       PRIOR (DATERR)                               
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   PBILLDT,WORK+10           YM                                     
         B     VR245                                                            
*                                                                               
VR245    XC    USERDATA,USERDATA                                                
         OC    SVP1USER,SVP1USER   ANY "PRODUCT 1" INFO?                        
         BZ    VR247                NO, DO "PRODUCT 2".                         
         LA    R2,PRDUSR1H                                                      
         ST    R2,AUSR             AUSR=A(INPUT FIELD).                         
         MVC   UTYPE,SVP1TYPE      TYPE.                                        
         MVC   LEN,SVP1LEN         LENGTH.                                      
         MVC   FLAG1,SVP1FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP1FLG2      2ND FLAG                                     
         BAS   RE,EDTUSR                                                        
*                                                                               
VR247    MVC   PUSER1,USERDATA                                                  
         MVC   PRDUSR1,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVP2USER,SVP2USER   ANY "PRODUCT 2" INFO?                        
         BZ    VR248                NO, MOVE ON.                                
         LA    R2,PRDUSR2H                                                      
         ST    R2,AUSR             A(INPUT FIELD).                              
         MVC   UTYPE,SVP2TYPE      TYPE.                                        
         MVC   LEN,SVP2LEN         LENGTH.                                      
         MVC   FLAG1,SVP2FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP2FLG2      2ND FLAG                                     
         BAS   RE,EDTUSR                                                        
*                                                                               
VR248    MVC   PUSER2,USERDATA                                                  
         MVC   PRDUSR2,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
*  OPTIONS                                                                      
*                                                                               
         NI    POPT1,X'FF'-POPT1_NOBILL  TURN OFF FLAG IN CASE ERASED           
         NI    POPT1,X'FF'-POPT1_RFC     TURN OFF FLAG IN CASE ERASED           
         NI    POPT1,X'FF'-POPT1_THTR    TURN OFF THTH IN CASE ERASED           
         NI    POPT1,X'FF'-POPT1_THNT    TURN OFF THNT IN CASE ERASED           
*                                                                               
         LA    R2,PRDOPTSH           CALL SCANNER TO BREAK UP OPTIONS           
         CLI   5(R2),0                                                          
         BE    VR250                                                            
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         ZICM  R0,DMCB+4             # OF OPTIONS SCANNED                       
         ZIC   R4,DMCB+4                                                        
*                                                                               
         LA    R5,BLOCK                                                         
*                                                                               
VR248_50 DS    0H                                                               
         CLC   12(3,R5),=C'RFC'                                                 
         BE    VR249_05                                                         
*                                                                               
         ZICM  R1,0(R5)                                                         
         BCTR  R1,0                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BILL'                                                
         BNE   VR249_10                                                         
*                                                                               
VR249    DS    0H                                                               
         ZICM  R1,1(R5),1            LEN OF SECOND HALF OF FIELD                
         BZ    TRAPERR                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'YES'                                                 
         BE    VR250                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'NO'                                                  
         BNE   TRAPERR                                                          
         OI    POPT1,POPT1_NOBILL                                               
         B     VR249_20                                                         
*                                                                               
VR249_05 DS    0H                  RFC=Y?                                       
*                                                                               
         CLI   22(R5),C'N'                                                      
         BE    VR249_20                                                         
         CLI   22(R5),C'Y'                                                      
         BNE   TRAPERR                                                          
         OI    POPT1,POPT1_RFC                                                  
         B     VR249_20                                                         
*                                                                               
VR249_10 DS    0H                  THTR?                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'THTR'                                                
         BNE   VR249_15                                                         
         TM    POPT1,POPT1_THNT                                                 
         BO    TRAPERR                                                          
         OI    POPT1,POPT1_THTR                                                 
         B     VR249_20                                                         
*                                                                               
VR249_15 DS    0H                  THNT?                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'THNT'                                                
         BNE   TRAPERR                                                          
         TM    POPT1,POPT1_THTR                                                 
         BO    TRAPERR                                                          
         OI    POPT1,POPT1_THNT                                                 
*                                                                               
VR249_20 LA    R5,32(R5)           CHECK NEXT OPTION                            
         BCT   R4,VR248_50                                                      
*                                                                               
VR250    DS    0H                                                               
         MVC   PPROF(30),=30C'0'          SET DEFAULT PROFILE                   
         CLI   ACTNUM,ACTADD       IS IT ADD                                    
         BE    VR255               YES/GO AND ADD PRD TO CLIENT HEADER          
         CLI   ACTNUM,ACTSEL       IS IT SELECT/CHANGE                          
         BE    VR252                                                            
         CLI   ACTNUM,ACTCHA       IS IT CHANGE                                 
         BNE   VRXIT                                                            
VR252    CLC   DMDSKADD,KEY+14                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
         B     VRXIT                                                            
         EJECT                                                                  
*             NOW ADD PRODUCT TO CLTHDR                                         
VR255    MVC   SAVEKEY,KEY         SAVE KEY                                     
         LA    R1,SVCLIST                                                       
         CLC   QPRD,=C'POL'                                                     
         BNE   VR280                                                            
         MVI   PCODE+1,X'FF'                                                    
VR260    CLI   0(R1),0             ENDOF LIST                                   
         BE    VR270                                                            
         CLI   3(R1),X'FF'                                                      
         BE    VR400               POL ALREADY THERE                            
         LA    R1,4(R1)                                                         
         B     VR260                                                            
*                                                                               
VR270    MVC   0(3,R1),=C'POL'                                                  
         MVI   3(R1),X'FF'                                                      
         B     VR400                                                            
*                                                                               
VR280    XC    ELEM,ELEM           ARRAY OF PRODUCTS I = ITH PRD                
         MVI   ELEM+218,X'FF'      CAN'T USE 219                                
         MVI   ELEM+219,X'FF'      CAN'T USE 220                                
         MVI   ELEM+253,X'FF'      CAN'T USE 254                                
         MVI   ELEM+254,X'FF'      CAN'T USE 255                                
         MVI   PRDMAX,252                                                       
         MVI   PRDNPMAX,251                                                     
* CURRENTLY ONLY ALLOW 252 PRODUCTS FOR CLIENT PG4 AGENCY MVNY                  
         CLC   AGENCY,=CL2'DU'                                                  
         BNE   VR285                                                            
         CLC   PRDCLT(3),=CL3'PG4'                                              
         BE    VR287                                                            
VR285    MVI   ELEM+251,X'FF'      CAN'T USE 252                                
         MVI   ELEM+252,X'FF'      CAN'T USE 253                                
         MVI   PRDMAX,250                                                       
         MVI   PRDNPMAX,249                                                     
VR287    SR    R4,R4               USED TO COUNT PRDS IN CLIST                  
******   LA    R2,MAXPRDNP         MAX NON POL PRODUCTS                         
         ZIC   R2,PRDNPMAX         MAX NON POL PRODUCTS                         
*                                                                               
VR290    CLI   0(R1),0             END OF CLIST?                                
         BE    VR320                                                            
         CLI   3(R1),0             OVERFLOW PRODUCT                             
         BE    VR315                                                            
         CLI   3(R1),X'FF'         POL PRODUCT?                                 
         BNE   VR300                                                            
         MVC   0(3,R1),=3X'FF'     YES, FOR PROPER LAST ENTRY                   
******   LA    R2,MAXPRD           MAX PROD INCLUDING POL                       
         ZIC   R2,PRDMAX           MAX NON POL PRODUCTS                         
         B     VR310               OF BINSRCH TABLE                             
*                                                                               
VR300    SR    R5,R5                                                            
         IC    R5,3(R1)                                                         
         LA    R5,ELEM-1(R5)                                                    
         MVI   0(R5),X'FF'   SET USED FLAG                                      
VR310    LA    R4,1(R4)         BUMP PRD COUNTER                                
VR315    LA    R1,4(R1)                                                         
         B     VR290                                                            
*                                                                               
VR320    LA    R1,ELEM      FIND LOWEST UNUSED CODE                             
VR330    CLI   0(R1),0                                                          
         BE    VR340                                                            
         LA    R1,1(R1)                                                         
         B     VR330                                                            
*                                                                               
VR340    LA    R0,ELEM-1                                                        
         SR    R1,R0                                                            
         STCM  R1,3,PCODE                                                       
         CLI   PCODE,X'FD'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
VR350    MVC   WORK(3),QPRD                                                     
         MVC   WORK+3(1),PCODE+1               R4 HAS NUMBER OF PRDS            
         GOTO1 =V(BINSRCH),DMCB,(X'01',WORK),SVCLIST,(R4),4,(0,3),(R2),X        
               RR=RELO                                                          
         OC    DMCB(4),DMCB                                                     
         BNZ   VR360                                                            
         BAS   RE,OVPRDCT                                                       
         CLI   OVPRDSW,C'E'                                                     
         BE    FULLERR                 TOO MANY PRODUCTS                        
         CLI   OVPRDSW,C'Y'                                                     
         BE    VR400                   IS THIS AN OVERFLOW PRODUCT              
*                                                                               
VR360    CLI   DMCB,1                 PRD NOT IN LIST                           
         BE    VR370                                                            
         L     R1,DMCB          HAS ADDR OF PRD                                 
         MVC   PCODE+1(1),3(R1)                                                 
*                                                                               
VR370    LA    R1,SVCLIST                                                       
VR380    CLI   3(R1),0                                                          
         BE    VR400                                                            
         CLC   0(3,R1),=3X'FF'                                                  
         BE    VR390                                                            
         LA    R1,4(R1)                                                         
         B     VR380                                                            
*                                                                               
VR390    MVC   0(3,R1),=C'POL'          RESET TO POL                            
*                                                                               
VR400    DS    0H                                                               
*                       WRITE BACK CLTHDR                                       
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         L     R1,AIO1                                                          
         MVC   KEY+1(3),1(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
         L     R6,AIO2                                                          
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   OVPRDSW,C'Y'                                                     
         BNE   VR405                                                            
         TM    CINDS1,CIN1OVP                                                   
         BO    *+8                                                              
         BAS   RE,SENDALRT      SEND E-MAIL FIRST TIME ONLY                     
         OI    CINDS1,CIN1OVP                                                   
         B     VR450                                                            
*                                                                               
*  CHECK SVCLIST/SVCLIST2 IS NOT CORRUPTED                                      
*                                                                               
VR405    LA    R4,SVCLIST                                                       
         LA    RE,CLIST                                                         
         LA    RF,220                                                           
VR410    CLI   3(RE),0                                                          
         BE    VR420                                                            
         CLI   3(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'      FOR EVERY CLIST ENTRY A SVCLIST MUST EXIST             
         LA    RE,4(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,VR410                                                         
*                                                                               
VR420    LA    R4,SVCLIST2                                                      
         LA    RE,CLIST2                                                        
         LA    RF,35                                                            
VR430    CLI   3(RE),0                                                          
         BE    VR440                                                            
         CLI   3(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'      FOR EVERY CLIST ENTRY A SVCLIST MUST EXIST             
         LA    RE,4(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,VR430                                                         
*                                                                               
VR440    LA    R4,SVCLIST                                                       
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         CLI   SVCLIST2+3,0        MORE THEN 219 PRODUCTS?                      
         BE    VR450               NO                                           
         LA    R4,SVCLIST2                                                      
         LA    RE,CLIST2                                                        
         LA    RF,140                                                           
         MVCL  RE,R4                                                            
*                                                                               
VR450    GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
         MVC   KEY,SAVEKEY         RESET PROGREC READING IN AIO1                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
******   MVC   PLEN,=H'240'                                                     
         MVC   PLEN,=H'336'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
VRXIT    B     REQREC                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  AFTER PRODUCT DELETE CHECK TO SEE                                            
*  IF CLIENT STILL HAS OVERFLOW PRODUCTS                                        
*  ASSOCIATED WITH IT. IF NOT SET SWITCH TO RESET                               
*  STATUS ON THE CLIENT RECORD.                                                 
*  SAVEKEY+4 = CLIENT TO BE DELETED                                             
*=============================== EDTUSR ==============================*         
CHKOVSTA NTR1                                                                   
         MVI   OVPRDSW,0                                                        
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PLSTPSSV,R2                                                      
*                                                                               
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM(3),SAVEKEY+1                                              
         GOTO1 HIGH                                                             
         B     CHKOVS30                                                         
CHKOVS20 GOTO1 SEQ                                                              
CHKOVS30 CLC   KEY(5),KEYSAVE                                                   
         BNE   CHKOVSEX                                                         
* CHECK TO SEE IF THERE WILL STILL BE OVERFLOW PRODUCTS                         
* IN THE CLIENT AFTER THE PRODUCT IS DELETED                                    
         CLI   PLSTBPRD+1,0                                                     
         BNE   CHKOVS20                                                         
         CLC   PLSTPRD,SAVEKEY+4                                                
         BE    CHKOVS20                                                         
         MVI   OVPRDSW,C'Y'                                                     
*                                                                               
CHKOVSEX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  CHECK IF OVERFLOW PRODUCT CAN BE CREATED                                     
*=============================== EDTUSR ==============================*         
OVPRDCT  NTR1                                                                   
         MVI   OVPRDSW,0                                                        
*  CHECK IF AGENCY ALLOWED TO HAVE OVERFLOW PRODUCT                             
         LA    RE,OVAGYTAB                                                      
OVPRD010 CLC   0(2,RE),=CL2'FF'                                                 
         BE    OVPRD030                                                         
         CLC   0(2,RE),AGENCY                                                   
         BNE   OVPRD020                                                         
         OC    2(2,RE),2(RE)        IS THERE A CLIENT RESTRICTION               
         BZ    OVPRD030                                                         
         CLC   2(2,RE),BCLT                                                     
         BE    OVPRD030                                                         
OVPRD020 LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         BE    OVPRDERR                                                         
         B     OVPRD010                                                         
*                                                                               
*  CHECK TO SEE IF MAXIMUM PRODUCTS ALREADY ADDED                               
*                                                                               
*                                                                               
OVPRD030 LA    R3,TOTPRDS          CHECK FOR ANY INPUT                          
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PLSTPSSV,R2                                                      
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM(3),SAVEKEY+1                                              
         GOTO1 HIGH                                                             
         B     OVPRD050                                                         
         DROP  R2                                                               
*                                                                               
OVPRD040 GOTO1 SEQ                                                              
OVPRD050 CLC   KEY(5),KEYSAVE                                                   
         BNE   OVPRDEX                                                          
         BCT   R3,OVPRD040                                                      
         B     OVPRDERR                                                         
*                                                                               
OVPRDEX  L     R6,AIO                                                           
         USING PKEY,R6                                                          
         MVI   OVPRDSW,C'Y'                                                     
         XC    PCODE,PCODE                                                      
         B     EXIT                                                             
*                                                                               
OVPRDERR MVI   OVPRDSW,C'E'                                                     
         B     EXIT                                                             
*                                                                               
*  OVAGYTAB  BYTE 1   AGENCY                                                    
*  OVAGYTAB  BYTE 2-3 CLIENT IF RESTRICTED                                      
OVAGYTAB DC    CL2'FF',XL2'0000'     ALL AGENCY'S                               
         DC    CL2'SJ',XL2'0000'     SJR                                        
         DC    CL2'S5',XL2'0000'     STW                                        
         DC    CL2'*B',XL2'0000'     DDSB                                       
         DC    CL2'*1',XL2'0000'     DDS1                                       
         DC    CL2'M2',XL2'0000'     MESEC                                      
         DC    CL2'TH',XL2'0000'     DFZSEC                                     
         DC    CL2'MC',XL2'0000'     MCSEC                                      
         DC    CL2'GZ',XL2'0000'     GMPDE                                      
         DC    CL2'ES',XL2'0000'     RACY                                       
         DC    CL2'H7',XL2'0000'     MSNY                                       
         DC    CL2'DR',XL2'BCDC'     SMGTEST/PG3                                
         DC    CL2'YN',XL2'BD8F'     MEDIAEDGE/PMP                              
         DC    CL2'YN',XL2'B0A2'     MEDIAEDGE/MFC                              
         DC    CL2'YN',XL2'B009'     MEDIAEDGE/MAJ                              
         DC    CL2'DU',XL2'BCDE'     MVNYN/PG5                                  
         DC    CL2'MC',XL2'A5A9'     MCLOU/JNJ                                  
         DC    CL2'FM',XL2'C8E2'     FMSEC/SHC                                  
         DC    CL2'FM',XL2'C8E3'     FMSEC/SHD                                  
         DC    CL2'FM',XL2'A993'     FMSEC/KMT                                  
         DC    CL2'FM',XL2'A983'     FMSEC/KMD                                  
         DC    CL2'OO',XL2'D2AB'     OMDDDB/UVL                                 
         DC    X'00'                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  IF OVERFLOW PRODUCT ADDED                                                    
*  SEND ALERT VIA E-MAIL TO TELL OF CONDITION                                   
*=============================== SENDALRT =============================         
SENDALRT NTR1                                                                   
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMSG,R1),NOTEMSG                                          
         LA    R1,L'NOTEMSG(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(3,R1),QPRD                                                     
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMSG+11,ELEM)                       
         SPACE                                                                  
SNDALRTX B     EXIT                                                             
NOTEMSG  DC    C'AUTONOTE*NSIL,SMUR,PZIR,SCHT:OVERFLOW PRODUCTS ADDED '         
         EJECT                                                                  
***********************************************************************         
*         ON ENTRY:                                                             
*             AUSR = A(INPUT FIELD),                                            
*             LEN  = ALLOWABLE LENGTH OF INPUT,                                 
*             UTYPE = TYPE ALLOWABLE FOR INPUT,                                 
*             FLAG1 = FIRST FLAG,                                               
*             FLAG2 = SECOND FLAG.                                              
*         ON EXIT:                                                              
*             USERDATA = INPUTTED DATA OR NULLS.                                
*                                                                               
*=============================== EDTUSR ==============================*         
EDTUSR   NTR1                                                                   
         L     R3,AUSR                                                          
         CLI   5(R3),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR10            THERE IS INPUT, PROCESS IT                   
*                                                                               
         MVI   ERROR,MISSING       NO INPUT, ASSUME IT'S MISSING                
         TM    FLAG1,CFLGREQQ      WAS INPUT REQUIRED?                          
         BZ    XEDTUSR             NO, SO IT'S OK                               
         B     TRAPERR             YES IT WAS, SHOW ERROR                       
*                                                                               
EDTUSR10 MVI   ERROR,TOOLONG       ASSUME INPUT IS TOO LONG                     
         CLC   LEN,5(R3)           CHECK IF L(INPUT) IS VALID                   
         BL    TRAPERR             NOT VALID                                    
*                                                                               
         MVI   ERROR,INVALID       ASSUME INPUT IS NOT VALID.                   
         CLI   UTYPE,C' '          IS TYPE SUPPOSE TO BE "WILD"?                
         BNH   EDTUSR80                                                         
         CLI   UTYPE,C'C'          IS TYPE SUPPOSE TO BE CHARACTER?             
         BNE   EDTUSR60                                                         
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
*                                                                               
EDTUSR40 CLI   0(R4),C'0'          ALLOW ALL INPUT EXCEPT NUMBERS               
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   TRAPERR                                                          
*                                                                               
EDTUSR50 LA    R4,1(R4)            CHECK NEXT CHAR IN INPUT                     
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'          IS TYPE SUPPOSE TO BE NUMERIC?               
         BNE   EDTUSR70             NOPE, THEN IT'S S/B DATE                    
         BAS   RE,CHKNTYP           YES, SO IS THE INPUT NUMERIC?               
         BE    EDTUSR80              YEP, INPUT IS VALID.                       
         B     TRAPERR               NO, ERROR DETECTED.                        
*                                                                               
EDTUSR70 MVI   ERROR,INVDATE       ASSUME INVALID DATE FORMAT                   
         CLI   UTYPE,C'D'          IS UTYPE SUPPOSE TO BE DATE?                 
         BE    *+6                  YES, VALIDATE INPUT FOR DATE                
         DC    H'0'                 NO, SOMETHING IS AMISS.                     
         GOTO1 DATVAL,DMCB,(0,8(R3)),TDATE                                      
         OC    DMCB(4),DMCB        ANY ERRORS?                                  
         BZ    TRAPERR              YEP                                         
         L     R1,0(R1)            L'INPUT FIELD                                
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   TRAPERR                                                          
*                                                                               
EDTUSR80 ZIC   R1,5(R3)            R1=L(INPUT).                                 
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)   MOVE INPUT INTO USERDATA.                    
*                                                                               
XEDTUSR  XIT1                                                                   
         EJECT                                                                  
*-------------------- VALIDATE INPUT FOR NUMERICS --------------------*         
CHKNTYP  DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
CHKN10   LA    R5,VALDNTBL         R5-->TABLE OF VALID DIGITS                   
CHKN20   CLC   0(1,R4),0(R5)       MATCH DIGIT BY DIGIT                         
         BE    CHKN30               MATCHED,                                    
         LA    R5,1(R5)             ELSE, TRY NEXT CHAR IN TABLE                
         CLI   0(R5),C'*'          ARE WE AT END-OF-TABLE YET?                  
         BE    XCHKN                YEP, EXIT WITH CC<>0                        
         B     CHKN20               NO, WE STILL HAVE HOPE                      
CHKN30   LA    R4,1(R4)            MATCHED, CHECK NEXT CHAR IN INPUT            
         BCT   R1,CHKN10                                                        
*                                                                               
XCHKN    L     RE,SAVERE                                                        
         LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
VALDNTBL DC    C'0123456789-/*'                                                 
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* GENERATE REQUEST RECORD                                                       
***********************************************************************         
REQREC   XC    REC(150),REC                                                     
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),PRDMED                                                   
         MVC   5(3,R1),PRDCLT                                                   
         OC    5(3,R1),SPACES                                                   
         MVC   11(3,R1),PRDPRD                                                  
         OC    11(3,R1),SPACES                                                  
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'P'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
GETPRD   NTR1                                                                   
         LA    R2,SVCLIST                                                       
GP5      CLI   3(R2),0             IF E-O-F CLIST                               
         BE    GPX                 SET TO UNDEFINED                             
         CLC   0(3,R2),QPRD                                                     
         BE    GP10                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP5                 RETURN TO LOOP                               
GP10     MVC   BPRD,3(R2)      SET 3 CHAR PRINTABLE PRD CODE                    
GPX      B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BCR   8,RE                                                             
FNDNXUF  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                END OF SCREEN                                
         EJECT                                                                  
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    CS4                                                              
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*                                                                               
LOCKOUT  MVI   ERROR,SECLOCK                                                    
         LA    R2,PRDCLTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
FULLERR  LA    R2,PRDPRDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR CLIENT LIST FULL'                       
         B     MYERR                                                            
*                                                                               
NOCLIENT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR NO CLIENT RECORD'                       
         B     MYERR                                                            
*                                                                               
ESTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR EST REC ATTACHED'                       
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*--LIST RECORDS AND PRINT RECORDS ROUTINE                                       
***********************************************************************         
         DS    0F                                                               
         DROP  RB                                                               
LISTREC  NMOD1 0,**C02LR*                                                       
         USING LISTREC,RB                                                       
         L     R9,4(R1)                                                         
         L     RA,8(R1)                                                         
         L     RC,12(R1)                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   0(R1),1                                                          
         BE    LR                                                               
         B     LRXIT                                                            
*                                                                               
LR       CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,15           SET NUM OF LIST LINES                        
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
*                                                                               
         XC    PREVCLT,PREVCLT                                                  
         NI    MYFLAG,X'FF'-YESSEC                                              
*                                                                               
         MVC   KEY(2),SVKEY                                                     
         MVI   KEYCHK,2                                                         
         OC    PRDCLT,PRDCLT       CLIENT                                       
         BZ    LR05                                                             
         CLI   PRDCLTH+5,2                                                      
         BL    LR02A                                                            
         OC    PRDCLT,SPACES                                                    
         GOTO1 CLPACK,DMCB,PRDCLT,KEY+2                                         
         MVI   KEYCHK,4                                                         
         B     LR05                                                             
LR02A    XC    FULL,FULL                                                        
         MVC   FULL(3),=3C'A'                                                   
         ZIC   R1,PRDCLTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FULL(0),PRDCLT                                                   
         GOTO1 CLPACK,DMCB,FULL,KEY+2                                           
         MVI   KEYCHK,4                                                         
         B     LR05                                                             
*                                                                               
LR03     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     ZIC   RE,KEYCHK                                                        
         BCTR  RE,0                                                             
         EX    RE,LRKEYCMP                                                      
         BNE   LRX                                                              
         CLC   KEY+4(3),=6X'00'                                                 
         BE    LR20                                                             
         CLC   KEY+7(6),=6X'00'                                                 
         BNE   LR20                                                             
LR30     OC    PRDPRD,PRDPRD       PRODUCT                                      
         BZ    LR35                                                             
         OC    PRDPRD,SPACES                                                    
         CLC   KEY+4(3),PRDPRD     PRODUCT                                      
         BNE   LR20                                                             
         SPACE                                                                  
LR35     GOTO1 GETREC                                                           
         BAS   RE,CLT0J            CONVERT 0/J CLIENT CODE                      
*                                                                               
         LA    R5,LISTAR                                                        
         USING PCLT,R5                                                          
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
*                                                                               
         TM    LSTFLTS,FLTTHTR     THEATRICAL?                                  
         BZ    *+12                                                             
         TM    POPT1,POPT1_THTR                                                 
         BZ    LR20                                                             
*                                                                               
         TM    LSTFLTS,FLTTHNT     THNT?                                        
         BZ    *+12                                                             
         TM    POPT1,POPT1_THNT                                                 
         BZ    LR20                                                             
*                                                                               
*&&DO                                                                           
         MVC   BCLT,PKEYCLT                                                     
         GOTO1 VLMTDACC                                                         
*&&                                                                             
         GOTO1 VLMTDACC,DMCB,PKEYAM,PKEYCLT                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   LR36                                                             
*                                                                               
         ICM   RF,3,PKEYCLT                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,KEY+2                                                       
         XC    KEY+4(9),KEY+4                                                   
         B     LR05                                                             
*                                                                               
LR36     GOTO1 CLUNPK,DMCB,(WORK2,PKEYCLT),PCLT     * CLIENT                    
         MVC   PPRD,PKEYPRD        * PRODUCT                                    
*                                                                               
         CLI   PACCT,X'FF'                                                      
         BNE   LR38                                                             
         UNPK  PCCDE(5),PACCT+1(3)                                              
         B     *+10                                                             
LR38     MVC   PCCDE(4),PACCT      * CLIENT/PRODUCT CODE                        
         CLI   PCLASS,0                                                         
         BE    LR40                                                             
         MVC   PCLAS(1),PCLASS                                                  
         MVI   PCLAS+1,C' '                                                     
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    LR40                NO                                           
         PACK  PCLAS(1),PCLASS     FIRST CLASS                                  
         NI    PCLAS,X'0F'                                                      
         OI    PCLAS,X'C0'         MAKE A - I                                   
         CLI   PCLASS+1,0                                                       
         BE    LR40                                                             
         MVC   PCLAS+1(1),PCLASS   2ND CLASS                                    
         NI    PCLAS+1,X'0F'                                                    
         OI    PCLAS+1,X'C0'       MAKE A - I                                   
*                                   AGENCY FEE                                  
LR40     CLC   PAGYFEE(2),=2X'00'                                               
         BE    LR45                                                             
         EDIT  (P2,PAGYFEE),(4,PAGYFE),2                                        
*                                                                               
LR45     MVC   PNME,PNAME            PRODUCT NAME                               
*                                                                               
         MVI   POVFL,X'40'           PRODUCT OVERFLOW INDICATOR                 
         OC    PCODE,PCODE                                                      
         BNZ   *+8                                                              
         MVI   POVFL,C'*'                                                       
*                                                                               
         CLI   PLOCK,X'40'           PRODUCT LOCK                               
         BNH   LR48                                                             
         MVC   PLOK(1),PLOCK                                                    
         GOTO1 DATCON,DMCB,(2,PLKDAT),(8,PLOK+2)                                
*                                                                               
LR48     CLI   MODE,PRINTREP                                                    
         BE    LR50                                                             
         GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
         SPACE                                                                  
LR50     DS    0H                                                               
         MVC   PBILLTO(30),PADDR1                                               
         XC    PBILLDAT(30),PBILLDAT                                            
         CLC   PBILLCOM(4),=6X'00'                                              
         BE    LR60                                                             
*                                                                               
         MVC   PBILLDAT(5),=CL5'CNET'                                           
         TM    PBILLBAS,X'50'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=CL5'NET'                                            
         TM    PBILLBAS,X'10'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=C'CGROS'                                            
         TM    PBILLBAS,X'40'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=C'GROSS'                                            
LR60     DS    0H                                                               
         EDIT  (4,PBILLCOM),(9,PCOMMPCT),4,FLOAT=-                              
         MVC   PCOMMBAS(5),=C'GROSS'                                            
         TM    PBILLBAS,X'01'                                                   
         BZ    LR70                                                             
         MVC   PCOMMBAS(5),=C'NET  '                                            
LR70     DS    0H                                                               
         XC    PEFFMON(30),PEFFMON                                              
         CLC   PBILLDT,=6X'00'                                                  
         BE    LR80                                                             
         GOTO1 DATCON,DMCB,(3,PBILLDT),(9,PEFFMON)                              
         DROP  R5                                                               
LR80     DS    0H                                                               
* PRINTING THE LINE                                                             
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PCLT(PLEND-PCLT),LISTAR   KEY DATA FROM LIST TO P                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    PCLT(PLEND-PCLT),PCLT     KEY DATA FROM LIST TO P                
         MVC   PBILLTO(30),PADDR2                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PBILLTO(30),PADDR3                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PBILLTO(30),PADDR4                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     LR20                GET NEXT RECORD                              
         SPACE                                                                  
LRX      DS    0H                                                               
         B     LRXIT                                                            
         DROP  R4,R6                                                            
LRXIT    XIT1                                                                   
LRKEYCMP CLC   KEY(2),KEYSAVE                                                   
         EJECT                                                                  
*                                                                               
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'NETWORK PRODUCT RECORDS'                                 
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=50C'-'                                              
         MVC   PPRD(3),=C'PRD'                                                  
         MVC   PPRD+132(3),=50C'-'                                              
         MVC   PCCDE(8),=C'C/P CODE'                                            
         MVC   PCCDE+132(8),=50C'-'                                             
         MVC   PCLAS(5),=C'CLASS'                                               
         MVC   PCLAS+132(5),=50C'-'                                             
         MVC   PAGYFE(3),=C'FEE'                                                
         MVC   PAGYFE+132(4),=50C'-'                                            
         MVC   PNME(12),=C'PRODUCT NAME'                                        
         MVC   PNME+132(20),=50C'-'                                             
         MVC   PLOK(4),=C'LOCK'                                                 
         MVC   PLOK+132(4),=50C'-'                                              
         MVC   PBILLTO+11(7),=C'BILL-TO'                                        
         MVC   PBILLTO+132(30),=50C'-'                                          
         MVC   PBILLDAT(9),=C'BILL DATA'                                        
         MVC   PBILLDAT+132(9),=50C'-'                                          
         MVC   PCOMMPCT+1(8),=C'COMM PCT'                                       
         MVC   PCOMMPCT+132(9),=50C'-'                                          
         MVC   PCOMMBAS(8),=C'COMM BAS'                                         
         MVC   PCOMMBAS+132(8),=50C'-'                                          
         MVC   PEFFMON(7),=C'EFF MON'                                           
         MVC   PEFFMON+132(8),=50C'-'                                           
         B     LRXIT                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*                                                                               
CLT0J    NTR1                                                                   
         MVC   MYSVKEY,KEY         SAVE AWAY PRG KEY                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLIENTD,R6                                                       
*                                                                               
         MVC   CKEYAM,MYSVKEY+1     AGY/MD                                      
         MVC   CKEYCLT,MYSVKEY+2    CLIENT                                      
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   WORK2(1),CPROF+6                                                 
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
CLT0JX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PCCDE    DS    CL5                                                              
         DS    CL4                                                              
PCLAS    DS    CL2                                                              
         DS    CL4                                                              
PAGYFE   DS    CL4                                                              
         DS    CL1                                                              
PNME     DS    CL20                                                             
         DS    CL1                                                              
POVFL    DS    CL1                                                              
         DS    CL1                                                              
PLOK     DS    CL4                                                              
         DS    CL1                                                              
PBILLTO  DS    CL30                                                             
         DS    CL1                                                              
PBILLDAT DS    CL9                                                              
         DS    CL1                                                              
PCOMMPCT DS    CL9                                                              
         DS    CL1                                                              
PCOMMBAS DS    CL8                                                              
         DS    CL1                                                              
PEFFMON  DS    CL8                                                              
         DS    CL1                                                              
PLEND    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC4D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C02 WORK AREA  *******                  
WORKAREA DS    0F                                                               
RELO     DS    F                                                                
SAVERE   DS    F                                                                
MYDMWRK  DS    CL8                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
MYSVKEY  DS    CL48                                                             
WORK2    DS    CL50                                                             
AGYALPHA DS    H                                                                
AUSR     DS    A                                                                
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
KEYCHK   DS    XL1                                                              
UTYPE    DS    XL1                                                              
TDATE    DS    CL6                                                              
USERDATA DS    CL32                                                             
REC      DS    CL150                                                            
USERFLD1 DS    XL32                                                             
USERFLD2 DS    XL16                                                             
PREVCLT  DS    XL2                                                              
OVPRDSW  DS    CL1                                                              
DELPRDCD DS    CL1                                                              
MYFLAG   DS    XL1                                                              
LSTFLTS  DS    XL1                                                              
FLTTHTR  EQU   X'01'               LIST THEATRICAL                              
FLTTHNT  EQU   X'02'               LIST THNT                                    
PRDMAX   DS    XL1                                                              
PRDNPMAX DS    XL1                                                              
YESSEC   EQU   X'01'               THIS CLIENT HAS SECURITY                     
         EJECT                                                                  
CLIENTD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRODD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
SCAND    DSECT                                                                  
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154NESFM02   04/06/20'                                      
         END                                                                    
