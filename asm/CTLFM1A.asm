*          DATA SET CTLFM1A    AT LEVEL 145 AS OF 05/01/02                      
*PHASE TA021AA,+0                                                               
*                                                                               
*PROGRAM LOGIC:                                                                 
*      - THE ACTUAL MARKET LISTS ARE KEPT IN SAVED STORAGE AT THE               
*        BOTTOM OF THE TWA.                                                     
*      - WHEN ACTION=CHANGE, RECORD WILL ONLY BE WRITTEN OUT TO THE             
*        FILE IF THERE WERE NO ERRORS,(ERRFLG=0), AND THE ENTER                 
*        KEY WAS HIT. IF ERRORS WERE PRESENT AND PF2 HIT, THE MKT               
*        CHANGE SCREEN WILL APPEAR AND LIST WILL GET UPDATED, UPON              
*        RETURN TO THE MAIN SCREEN, FIELDS WILL BE RE-VALIDATED.                
*      - IF USER IS CONNECTED TO THE APPROPRIATE AGENCY, THE MKT OVR'S          
*        WILL GET VALIDATED, OTHERWISE, ANY INPUT IS ALLOWED                    
*                                                                               
* CHANGES:                                                                      
* --------                                                                      
* 08/29/00  BPOO  ADDDBEXTEND  FOR USER BINARY AGENCY CODE                      
* 08/18/00  BPOO  CHANGE GETALPHA  DATAMGR CALL LOGIC                           
*                                                                               
* 12/16/92 (+/-) - MAYA - PF2 MKT SCREEN CHANGE, 2 ELEMENTS ALLOWED             
*        UP TO 2-MKTS ALLOWED FOR EACH MKT LIST.                                
*                                                                               
* 12/30/92  MAYA - ADDED PASSWORD FEATURE. - FIXED BUG WITH PF2->MAIN           
*        SCREEN LIST TRANSFER. MUST FILTER OUT 0000 MKTS FROM COUNT.            
*        RESTR23 FIXES PROBLEM.                                                 
*                                                                               
* *********************************************************************         
         EJECT                                                                  
*                                                                               
*INCLUDE SCINKEY                                                                
*INCLUDE XSORT                                                                  
*INCLUDE NSIWEEK                                                                
*INCLUDE NETWEEK                                                                
*INCLUDE NETUNWK                                                                
         TITLE 'CTLFM1A <==> TA021A - DEMO CONTROL RECORDS : CTLFM1A'           
CTLFM1A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFMZ**,RA                                          
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(TEMP W/S)                               
         L     R2,ATWA                                                          
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         RELOC RELO                                                             
         L     RF,RELO                                                          
         L     RE,=V(SUBR01)                                                    
         AR    RE,RF                                                            
         ST    RE,VSUBR01          A(1ST SUBRTNE POOL)                          
         L     RE,=V(SUBR02)                                                    
         AR    RE,RF                                                            
         ST    RE,VSUBR02          A(2ND SUBRTNE POOL)                          
         L     RE,=V(SUBR03)                                                    
         AR    RE,RF                                                            
         ST    RE,VSUBR03          A(3RD SUBRTNE POOL)                          
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   DEMAND,CDEMAND-COMFACSD(RF)    A(DEMAND)                         
         MVC   CALLOV,CCALLOV-COMFACSD(RF)    A(CALLOV)                         
         MVC   GETDAY,CGETDAY-COMFACSD(RF)    A(GETDAY)                         
         MVC   ADDAY,CADDAY-COMFACSD(RF)      A(ADDAY)                          
         MVC   PERVAL,CPERVAL-COMFACSD(RF)    A(PERVAL)                         
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         SR    RE,RE                                                            
         ZICM  RF,TIOBCURS,(3)     CALC ADDR OF CURSOR                          
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         ST    RF,CURPOS           SAVE CURSOR POSITION (ADDRESS)               
         OI    BASTYPEH+6,X'01'    ALWAYS SET SCRN TO MODIFIED                  
*                                                                               
         ZIC   RF,TIOBAID                                                       
         CLI   TIOBAID,12          CONVERT PF13-24 TO PF1-12                    
         BNH   *+8                                                              
         SH    RF,=H'12'                                                        
         STC   RF,PFKEY                                                         
         DROP  R1                                                               
*                                                                               
         LA    RF,MYIO                                                          
         ST    RF,AMYIO                                                         
         LH    RF,=Y(BUFF-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,ABUFF                                                         
         LH    RF,=Y(BUFF2-WORKD)                                               
         LA    RF,WORKD(RF)                                                     
         ST    RF,ABUFF2                                                        
*                                                                               
CNTL10   CLC   RMKMKID(3),=C'MKT'   CURRENT SCREEN IS MKT LIST SCREEN?          
         BE    MKTDSP                                                           
         CLC   RMKMKID(3),=C'SPT'   CURRENT SCREEN IS SPT LIST SCREEN?          
         BE    SPLST                                                            
         CLC   RMKMKID(3),=C'INF'   CURRENT SCREEN IS INFO LIST SCREEN?         
         BNE   KEYVAL              REGULAR SCREEN - VAL KEY                     
         CLI   PFKEY,1                                                          
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
         B     INFO                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
*              VALIDATE KEY FIELDS                                              
* ********************************************************************          
*                                                                               
KEYVAL   DS    0H                                                               
         OI    BASTYPEH+6,X'80'                                                 
         L     R4,AREC                                                          
         USING CTRREC,R4                                                        
         LR    R3,R2                                                            
         USING LFMSAVE,R3                                                       
         XC    CTRKEY,CTRKEY                                                    
         MVI   CTRKTYP,CTRKTEQU                                                 
         MVC   TMPACT,ACTN         SAVE TYPE ACTION REQUESTED                   
*                                                                               
         OI    DEMPSWDH+6,X'80'                                                 
*                                                                               
         MVI   PSWOK,0                                                          
         GOTO1 AFVAL,DEMPSWDH      PASSWORD MUST BE INPUT                       
         BZ    KEYV1A                                                           
         CLI   FLDH+5,6                                                         
         BNE   EIIF                                                             
         GOTO1 VDATCON,DMCB,(5,0),(20,DUB)     YYMMDD                           
         LA    R1,DUB+7                                                         
         LA    RE,DUB1                                                          
         LA    RF,6                                                             
         MVC   0(1,RE),0(R1)       REVERSE DATE INTO DUB1                       
         LA    RE,1(RE)                                                         
         BCTR  R1,0                                                             
         BCT   RF,*-12                                                          
         PACK  DUB,DUB1(6)                                                      
         ZAP   DUB1,=P'999999'                                                  
         SP    DUB1,DUB            AND GET 9'S COMPLEMENT                       
         UNPK  DUB(6),DUB1                                                      
         OI    DUB+5,X'F0'                                                      
         CLC   DUB(6),FLD          TEST IF PASSWORD VALID                       
         BE    KEYV1                                                            
*                                  VALIDATE SOURCE CODE                         
KEYV1A   CLI   ACTN,DISPLAY        ACTION CHANGE REQUIRES A PASSWORD            
         BNE   EIIF                                                             
         XC    DEMPSWD,DEMPSWD                                                  
         MVI   PSWOK,C'N'          INVALID/NO PASSWORD                          
*                                                                               
KEYV1    OI    DEMSRCH+6,X'80'                                                  
         GOTO1 AFVAL,DEMSRCH                                                    
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     RE,=A(SRCTAB)           LOOK-UP SOURCE IN TABLE                  
         A     RE,RELO                                                          
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     KEYV2                                                            
         MVC   CTRKSRC,L'SRCTAB-1(RE)                                           
         CLC   DEMSRC,0(RE)                                                     
         BE    *+14                                                             
         MVC   DEMSRC,0(RE)                                                     
         OI    DEMSRCH+6,X'80'                                                  
*                                  VALIDATE MEDIA (SUB-FILE) CODE               
         GOTO1 AFVAL,DEMSUBFH                                                   
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     RE,=A(MEDTAB)                                                    
         A     RE,RELO                                                          
KEYV3    CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV3                                                            
         CLC   DEMSUBF,1(RE)                                                    
         BE    *+14                                                             
         MVC   DEMSUBF,1(RE)                                                    
         OI    DEMSUBFH+6,X'80'                                                 
         MVC   CTRKMED,0(RE)                                                    
*                                  ENSURE SOURCE/MEDIA IS VALID                 
         L     RE,=A(SMTAB)                                                     
         A     RE,RELO                                                          
KEYV3A   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   0(2,RE),CTRKSRC                                                  
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     KEYV3A                                                           
*                                  VALIDATE AGENCY CODE                         
         MVI   CNCT,0                                                           
         L     RF,ATWA                                                          
         CLC   =C'++',14(RF)     IF LOGON=DEMO, NO SPOT SYSTEM                  
         BNE   *+8                                                              
         MVI   CNCT,X'80'          NO SPOT SYS FOR DEMO ->NO VALIDATN           
*                                                                               
         MVC   CTRKAGY,=X'FFFF'    VALIDATE AGENCY CODE                         
*  BPOO   8/24/00                                                               
         MVI   USIDFLAG,USIDALP                                                 
*                                                                               
         CLC   =C'U=',DEMAGY                                                    
         BNE   KEYV3D                                                           
         GOTO1 AFVAL,DEMAGYH                                                    
         XC    DUB,DUB                                                          
         ZIC   RE,DEMAGYH+5                                                     
         AHI   RE,-2            MINUS U= 2 CHARS                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DEMAGY+2(0)                                                  
         CVB   R0,DUB                                                           
         STCM  R0,3,CTRKAGY                                                     
         MVC   SVBINAGY,CTRKAGY                                                 
         GOTO1 VSUBR03,DMCB,('VALBUSR',(R9)),(RC)                               
         BNE   EIIF                                                             
         MVI   USIDFLAG,USIDBIN                                                 
         MVC   SVSCRNID,DEMAGY                                                  
         B     KEYV3E                                                           
*                                                                               
KEYV3D   DS    0H                                                               
         XC    SVAPHAGY,SVAPHAGY                                                
         GOTO1 AFVAL,DEMAGYH                                                    
         BZ    KEYV4                                                            
         CLI   FLDH+5,2                                                         
         BNE   EIIF                                                             
         MVC   CTRKAGY,FLD                                                      
         MVC   SVAPHAGY,CTRKAGY    SAVE  ALPHA AGENCY AND USE SVAPHAGY          
*                                  FROM NOW ON                                  
KEYV3E   L     RF,ATWA                                                          
******   CLC   CTRKAGY,14(RF)      ARE WE CNCTED TO THIS AGY                    
         CLC   SVAPHAGY,14(RF)     ARE WE CNCTED TO THIS AGY                    
         BE    *+8                                                              
         MVI   CNCT,X'80'          NO                                           
*                                  VALIDATE LOOKUP CODE                         
KEYV4    DS    0H                                                               
         MVI   CTRKCODE,X'FF'                                                   
         GOTO1 AFVAL,DEMCODEH                                                   
         BZ    KEYV5                                                            
         CLI   DEMCODE,X'C1'                                                    
         BL    EIIF                                                             
         CLI   DEMCODE,X'E9'                                                    
         BH    EIIF                                                             
         MVC   CTRKCODE,FLD                                                     
*                                  VALIDATE CLIENT CODE                         
KEYV5    MVC   CTRKCLI,=X'FFFFFF'                                               
         GOTO1 AFVAL,DEMCLIH                                                    
         BZ    KEYV6                                                            
******   CLC   CTRKAGY,=X'FFFF'                                                 
         CLC   SVAPHAGY,=X'FFFF'                                                
         BE    EIIF                                                             
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         MVC   CTRKCLI,FLD                                                      
*                                  VALIDATE START BOOK                          
KEYV6    GOTO1 AFVAL,DEMBOOKH                                                   
         BNZ   KEYV6D                                                           
         CLI   ACTN,DISPLAY        IF ACTN=DISPLAY AND                          
         BE    KEYV7               FIELD IS BLANK, USE LATEST BOOK              
         B     EXIT                                                             
KEYV6D   ZIC   R1,FLDH+5           SET PARMS FOR DTTOWK SUBRTNE:                
         STC   R1,WORK+2            L(INPUT BOOK)                               
         BCTR  R1,0                                                             
         LR    R0,R1               HOLD ONTO L'INPUT-1 IN R0                    
         EXMVC R1,DUB,FLD           INPUT BOOK                                  
         MVC   WORK(2),CTRKSRC      SOURCE/MEDIA                                
         GOTO1 VSUBR01,DMCB,('DTTOWE',(R9)),(RC)                                
         BNE   EIDF                                                             
         MVC   CTRKBOOK,MYHALF                                                  
         XC    CTRKBOOK,=X'FFFF'                                                
         SPACE 1                                                                
         GOTO1 VSUBR02,DMCB,('WKTODE',(R9)),(RC)                                
         ZIC   R1,DUB2             REFORMAT BOOK INPUT,                         
         BCTR  R1,0                                                             
         CR    R0,R1                IF NECESSARY                                
         BNE   KEYV6E                                                           
         EXCLC R1,DUB,FLD                                                       
         BE    KEYV7                                                            
KEYV6E   XC    DEMBOOK,DEMBOOK     IT'S NECESSARY                               
         EXMVC R1,DEMBOOK,DUB                                                   
         OI    DEMBOOKH+6,X'80'                                                 
         EJECT                                                                  
KEYV7    MVC   WORK(2),CTRKBOOK    SAVE START BOOK                              
         MVC   KEY,CTRKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,DEMSRCH                                                       
         ST    R1,FADR                                                          
*                                                                               
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   *+18                SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU FOR UPDATABLE ACTIONS                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
*                                                                               
         TM    DMCB+8,X'10'        WAS RECD FOUND?                              
         BZ    KEYV9               YES-BRANCH                                   
         CLI   ACTN,ADD            REC NOT FOUND- VALID FOR ADD                 
         BNE   *+12                                                             
         MVI   MYMODE,VALQ         VALIDATE FIELDS                              
         B     KEYVX                                                            
         CLI   ACTN,CHANGE         NOT VALID FOR CHANGE                         
         BE    ERNF                                                             
         CLC   KEY(23),CTRKEY      LATEST BOOK CAN BE EQUAL FOR DISPLAY         
         BNE   ERNF                                                             
*                                                                               
KEYV9    CLI   ACTN,ADD            REC FOUND. NOT VALID FOR ADD                 
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORDS MAY ONLY BE DSPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BNE   *+12                                                             
         MVI   MYMODE,VALQ                                                      
         B     KEYVX                                                            
         CLC   WORK(2),CTRKBOOK                                                 
         BNE   *+12                                                             
         MVI   MYMODE,DISPQ                                                     
         B     KEYVX                                                            
         MVC   WORK(2),CTRKSRC     REPLACE GIVEN BOOK WITH REAL BOOK            
         MVC   MYHALF,CTRKBOOK                                                  
         XC    MYHALF,=X'FFFF'                                                  
         XC    DEMBOOK,DEMBOOK                                                  
         GOTO1 VSUBR02,DMCB,('WKTODE',(R9)),(RC)                                
         ZIC   R1,DUB2             DUB2(1) = L(OUTPUT DATE)                     
         BCTR  R1,0                                                             
         EXMVC R1,DEMBOOK,DUB      DUB = OUTPUT DATE                            
         OI    DEMBOOKH+6,X'80'                                                 
         MVI   MYMODE,DISPQ                                                     
*                                                                               
KEYVX    MVC   RSTAT,CTRSTAT       SAVE KEY IN SVTD                             
         CLI   PFKEY,1             SAVE KEY IN SVTD                             
         BE    *+12                                                             
         CLI   PFKEY,2                                                          
         BNE   KEYVX2                                                           
*        BAS   RE,SETBUF           DETERMINE WHICH MKT LIST WANTED              
*         GOTO1 =A(SETBUF),DMCB,(R2),(R3),(R9),(RC),RR=RELO                     
         GOTO1 VSUBR02,DMCB,('SETBUE',(R9)),(RC)                                
*                                                                               
KEYVX2   CLI   MYMODE,VALQ                                                      
         BE    DATAVAL                                                          
         CLI   MYMODE,DISPQ                                                     
         BE    DISPREC                                                          
         DROP  R4                                                               
         DROP  R3                                                               
         EJECT                                                                  
* *********************************************************************         
*              DISPLAY RECORD                                                   
* *********************************************************************         
DISPREC  DS    0H                                                               
         L     R4,AREC             R4=DCON RECORD                               
         USING CTRREC,R4                                                        
*                                                                               
         TWAXC DEMLUPRH            CLEAR SCREEN                                 
         LA    R5,CTRDATA          R5=A(ELEMENT)                                
         DROP  R4                                                               
*                                                                               
         LA    RE,SCRSTR           CLEAR LISTS                                  
         LH    RF,=Y(LISTQ)                                                     
         XCEF                                                                   
*                                                                               
DISP2    CLI   0(R5),0             TEST E-O-R                                   
         BE    DISP20                                                           
         CLI   0(R5),X'03'         DEMO CONTROL ELEMENT                         
         BE    DISP6                                                            
         CLI   0(R5),CTRMECDQ      MARKET CONTROL ELEMENT                       
         BE    DISP8                                                            
*                                                                               
DISP4    ZIC   R1,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  DISPLAY LOOKUP CONTROLS                      
DISP6    TM    2(R5),X'80'                                                      
         BZ    *+14                                                             
         MVC   DEMLUPR(19),=C'USE AGENCY DEFAULTS'                              
         B     DISP4                                                            
*                                  FORMAT SCAN BLOCK FOR DISPLAY                
         LA    R4,SCANTBL          R4=A(SCAN TABLE)                             
         SR    R6,R6               R6=N'ENTRIES IN TABLE                        
         L     R7,=A(PARMTAB)      R7=A(PARM TABLE)                             
         A     R7,RELO                                                          
*                                                                               
DISP6A   CLI   0(R7),0             TEST E-O-T                                   
         BE    DISP6E                                                           
         SR    R8,R8                                                            
         ICM   R8,7,11(R7)                                                      
         A     R8,RELO             R8=A(KEYWORD TABLE)                          
*                                                                               
DISP6B   CLI   0(R8),0             TEST E-O-T                                   
         BE    DISP6D                                                           
         ZIC   RE,8(R8)                                                         
         LA    RE,2(R5,RE)         RE=A(CONTROL BYTE IN ELEMENT)                
         ZIC   RF,9(R8)                                                         
         EX    RF,*+8              RF=OR MASK                                   
         B     *+8                                                              
         TM    0(RE),0             TEST IF BIT ON                               
         BZ    DISP6C                                                           
*                                  YES - FORMAT SCAN ENTRY                      
         MVI   0(R4),C' '                                                       
         MVC   1(L'SCANTBL-1,R4),0(R4)                                          
         MVC   0(8,R4),0(R7)                                                    
         LA    R1,7(R4)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
         MVC   2(8,R1),0(R8)                                                    
         LA    R4,L'SCANTBL(R4)                                                 
         LA    R6,1(R6)                                                         
         B     DISP6D                                                           
*                                  BUMP TO NEXT KEY TABLE ENTRY                 
DISP6C   LA    R8,L'KEYTAB(R8)                                                  
         B     DISP6B                                                           
*                                  BUMP TO NEXT PARM TABLE ENTRY                
DISP6D   LA    R7,L'PARMTAB(R7)                                                 
         B     DISP6A                                                           
*                                  FORMAT SCAN BLOCK INTO TWA                   
DISP6E   LTR   R6,R6                                                            
         BZ    DISP4                                                            
         L     RF,=V(SCINKEY)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(3,DEMLUPRH),(32,SCANTBL),(R6)                         
         B     DISP4                                                            
*                                                                               
*DISP8    BAS   RE,STRMKT           SAVE MKTS IN STRG AREA                      
DISP8    GOTO1 VSUBR01,DMCB,('STRMKE',(R9)),(RC)                                
         B     DISP4                                                            
*                                                                               
*ALL ELEMENTS HAVE BEEN PROCESSED                                               
*DISPLAY MKTS FROM LISTS BUILT IN SAVED STORAGE                                 
*                                                                               
DISP20   LA    R5,NVMKT                                                         
         LA    R7,3                 DO ALL LISTS                                
DISP25   CLI   0(R5),0                                                          
         BE    DISP27                                                           
*         GOTO1 =A(DSPMKT),DMCB,(R2),(R3),(R9),(RC),RR=RELO                     
         GOTO1 VSUBR02,DMCB,('DSPMKE',(R9)),(RC)                                
DISP27   AH    R5,=Y(LISTSZ)        DISPL TO NEXT MKT LIST                      
         BCT   R7,DISP25                                                        
*                                                                               
         LA    R1,DEMLUPRH         SET NEXT ACTION AND EXIT                     
         ST    R1,FADR                                                          
         L     R4,AREC                                                          
         USING CTRREC,R4                                                        
         MVI   NACTN,0                                                          
         CLI   PSWOK,C'N'          WAS PASSWORD OKAY?                           
         BNE   DISP28              NO, ONLY ALLOW CHANGE                        
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(16),=C'RECORD DISPLAYED'                                  
         OI    BASHDRH+6,X'80'                                                  
         TM    CTRSTAT,X'80'                                                    
         BNO   DISP29                                                           
         MVC   BASHDR+16(11),=C'-IS DELETED'                                    
         MVI   NACTN,OKRES                                                      
         B     DISP29                                                           
*                                                                               
DISP28   TM    CTRSTAT,X'80'                                                    
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         DROP  R4                                                               
*                                                                               
DISP29   CLI   PFKEY,1             INFO LIST REQUESTED?                         
         BNE   DISP30              NO                                           
         MVC   LKEY-LFMSAVE(L'LKEY,R2),KEY                                      
*         BAS   RE,CALLSCR          YES, LOAD LST SCREEN                        
         GOTO1 VSUBR01,DMCB,('CALLSCE',(R9)),(RC)                               
         MVC   RMKMKID(3),=C'INF'                                               
         B     INFO                                                             
*                                                                               
DISP30   CLI   PFKEY,2             MKT LIST REQUESTED?                          
         BNE   DISPX                                                            
         LA    RE,SAVESTR                                                       
         USING SVTD,RE                                                          
         LA    R1,NVMKT                                                         
         CLI   SVTLSTYP,CTRMTPV    VALID MKT LISTING?                           
         BE    DISP35                                                           
         LA    R1,NIMKT            INVALID LISTING                              
         CLI   SVTLSTYP,CTRMTPI    WAS MKT OVR LIST REQ?                        
         BE    DISP35              NO                                           
         LA    R1,NOMKT            OVERIDE LISTING?                             
*                                                                               
DISP35   ST    R1,AMKT                                                          
         CLI   0(R1),0             MAKE SURE LIST IS NOT EMPTY                  
         BZ    DISP40                                                           
         MVC   SVTELLN,0(R1)       # MKTS IN LIST                               
         MVC   SVTMTYP,1(R1)       MKT TYPE                                     
         ZIC   R1,SVTELLN          COMPUTE LENGTH OF MKT LIST                   
         SLL   R1,1                R1=#BYTES TO MOVE                            
         LA    RF,SVTMKTS          DESTINATION                                  
         L     RE,AMKT             SOURCE                                       
         LA    RE,2(RE)                                                         
         MOVE  ((RF),(R1)),(RE)    COPY MKTS TO SAVED LIST                      
         DROP  RE                                                               
*                                                                               
DISP40   TM    CNCT,X'01'+X'80'    MKT OVR LIST REQ & NOT CNCTD?                
         BNO   DISP45              NO                                           
         XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(35),=C'RECONNECT TO AGENCY FOR MKT LISTING'               
         OI    BASHDRH+6,X'80'                                                  
         LA    R1,DEMAGYH                                                       
         ST    R1,FADR                                                          
         B     DISPX                                                            
*                                                                               
DISP45   MVC   LKEY-LFMSAVE(L'LKEY,R2),KEY                                      
*         BAS   RE,CALLSCR          SAVE CURRENT SCRN AND LOAD LST SCRN         
         GOTO1 VSUBR01,DMCB,('CALLSCE',(R9)),(RC)                               
         LA    RE,SAVESTR                                                       
         MVC   RMKMKID(3),=C'SPT'                                               
         CLI   SVTLSTYP-SVTD(RE),X'01'   MKT OVERIDE LIST REQUESTED?            
         BE    SPLST00             YES- READ SPOT FILE & LIST MKTS              
         MVC   RMKMKID(3),=C'MKT'                                               
         B     MKDSP00             NO - GET MKTS FROM DEMAND & LIST             
*                                                                               
DISPX    B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
*              ADD/CHANGE RECORD                                                
* *********************************************************************         
*                                                                               
DATAVAL  DS    0H                                                               
         L     R4,AREC                                                          
         USING CTRREC,R4                                                        
         MVI   ERRFLG,0                                                         
         MVI   TEMP,0              BUILD VIRGIN RECORD & ACTIVITY ELEM.         
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE LOOKUP RULES                        
         XC    TEMP,TEMP                                                        
         MVI   TEMP,X'03'                                                       
         MVI   TEMP+1,7                                                         
         XC    OPTIONS,OPTIONS     CLEAR OPTIONS                                
         GOTO1 AFVAL,DEMLUPRH                                                   
         BZ    DATAV16                                                          
*                                  TEST FOR SPECIAL INPUT                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'USE AGENCY DEFAULTS'                                   
         BNE   DATAV2                                                           
         OI    TEMP+2,X'80'                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DATAV30                                                          
*                                                                               
DATAV2   LA    R6,DEMLUPRH         R6=A(FIRST INPUT FIELD)                      
         LA    R7,2                R7=N'INPUT FIELDS                            
         B     DATAV6                                                           
*                                                                               
DATAV4   LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV14                                                          
*                                                                               
DATAV6   GOTO1 VSCANNER,DMCB,FLDH,(20,SCANTBL)                                  
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF ENTRIES                       
         MVI   FNDX,1                                                           
         LA    R8,SCANTBL          R8=A(SCAN TABLE ENTRY)                       
*                                                                               
DATAV8   CLC   FNDX,NLINES                                                      
         BH    DATAV14                                                          
         CLI   0(R8),1             L'LHS                                        
         BL    EIIF                                                             
         CLI   0(R8),8             L'LHS                                        
         BH    EIIF                                                             
*                                  LOOK-UP PARM IN TABLE                        
         L     RE,=A(PARMTAB)      RE=A(PARMTAB)                                
         A     RE,RELO                                                          
         ZIC   RF,0(R8)                                                         
         BCTR  RF,0                RF=L'INPUT PARM-1                            
*                                                                               
DATAV10  CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R8)                                                   
         BE    *+12                                                             
         LA    RE,L'PARMTAB(RE)                                                 
         B     DATAV10                                                          
         ZIC   RF,8(RE)            TEST IF PARM DUPLICATED                      
         LA    RF,OPTIONS-1(RF)                                                 
         CLI   0(RF),0                                                          
         BNE   EDIF                                                             
         MVI   0(RF),1             SET PARM INPUT                               
         CLC   1(1,R8),9(RE)       CHECK L'KEYWORD                              
         BL    EFTS                                                             
         CLC   1(1,R8),10(RE)                                                   
         BH    EFTL                                                             
*                                  LOOK-UP KEYWORD IN TABLE                     
         SR    RF,RF                                                            
         ICM   RF,7,11(RE)                                                      
         LR    RE,RF                                                            
         A     RE,RELO             RE=A(KEYWORD TABLE)                          
         ZIC   RF,1(R8)                                                         
         BCTR  RF,0                RF=L'INPUT KEYWORD-1                         
*                                                                               
DATAV12  CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),22(R8)                                                   
         BE    *+12                                                             
         LA    RE,L'KEYTAB(RE)                                                  
         B     DATAV12                                                          
         ZIC   RF,8(RE)                                                         
         LA    RF,TEMP+2(RF)       RF=A(CONTROL BYTE IN ELEMENT)                
         OC    0(1,RF),9(RE)       OR ON BIT VALUE                              
*                                  BUMP TO NEXT SCAN BLOCK ENTRY                
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R8,L'SCANTBL(R8)                                                 
         B     DATAV8                                                           
*                                  BUMP TO NEXT INPUT LINE                      
DATAV14  ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         TM    1(R6),X'20'         IGNORE PROT FIELDS                           
         BO    DATAV14                                                          
         BCT   R7,DATAV4                                                        
*                                  ADD DEMO CONTROL ELEMENT                     
DATAV16  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE VALID MARKET LIST                   
*         BAS   RE,GETMKTS          READ MKTS FROM DEMAND INTO BUFF             
         TM    CNCT,X'80'                                                       
         BO    DATAV16M                                                         
         GOTO1 VSUBR01,DMCB,('GETMKTE',(R9)),(RC)                               
DATAV16M XC    MRKTLST(2),MRKTLST                                               
         LA    R6,NVMKT            DEST OF SAVED LIST                           
         ST    R6,AMKT             SAVE ADDR OF LIST FOR VALMRKT                
         LA    R6,DEMVMARH         A(1ST TWA FIELD)                             
         LA    R7,4                # TWA FIELD TO PROCESS                       
         MVI   MKTTYPE,CTRMTPV                                                  
         CLI   PROTV,C'P'          IS FIELD PROTECTED?                          
         BE    DATAV16B                                                         
*         GOTO1 =A(VALMRKT),DMCB,(R2),(R9),(RC),RR=RELO                         
         GOTO1 VSUBR03,DMCB,('VALMRKE',(R9)),(RC)                               
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
*                                  VALIDATE INVALID MARKET LIST                 
DATAV16B LA    R6,NIMKT            DEST OF SAVED LIST                           
         ST    R6,AMKT             SAVE ADDR OF LIST FOR VALMRKT                
         LA    R6,DEMIMARH         A(1ST TWA FIELD)                             
         LA    R7,4                #TWA FIELD                                   
         MVI   MKTTYPE,CTRMTPI                                                  
         CLI   PROTI,C'P'          IS FIELD PROTECTED?                          
         BE    DATAV17                                                          
*         GOTO1 =A(VALMRKT),DMCB,(R2),(R9),(RC),RR=RELO                         
         GOTO1 VSUBR03,DMCB,('VALMRKE',(R9)),(RC)                               
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
*                                                                               
*DATAV17  BAS   RE,VALDUP           ANY DUPL MKTS IN VAL-INV LISTS?             
DATAV17  GOTO1 VSUBR01,DMCB,('VALDUE',(R9)),(RC)                                
*                                                                               
DATAV18  TM    CNCT,X'80'          ARE WE CNCTD TO CRCT SYSTEM?                 
         BO    DATAV20             NO-- DON'T TRY TO SWITCH                     
         L     RF,ACOMFACS         VALID MKT OVERIDE LIST                       
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,0                                                         
         BE    DATAV20                                                          
         CLI   DMCB+4,2                                                         
         BNE   DATAV19                                                          
         GOTO1 (RF),DMCB,=C'CON',0     SWITCH BACK TO CNTL SYSTEM               
DATAV19  XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(19),=C'SYSTEM SWITCH ERROR'                               
         B     EXIT                                                             
*                                                                               
DATAV20  XC    MRKTLST(2),MRKTLST                                               
         LA    R6,NOMKT            DEST OF SAVED LIST                           
         ST    R6,AMKT             SAVE ADDR OF LIST FOR VALMRKT                
         LA    R6,DEMOMARH         A(1ST TWA FIELD)                             
         LA    R7,7                # TWA FIELD TO PROCESS                       
         MVI   MKTTYPE,1                                                        
         CLI   PROTO,C'P'          IS FIELD PROTECTED?                          
         BE    DATAV21                                                          
*         GOTO1 =A(VALMRKT),DMCB,(R2),(R9),(RC),RR=RELO                         
         GOTO1 VSUBR03,DMCB,('VALMRKE',(R9)),(RC)                               
*                                                                               
DATAV21  CLI   NOMKT,0                                                          
         BE    *+16                                                             
         CLI   OTYPE,C'?'          MISSING/INVAID SOURCE                        
         BNE   *+8                                                              
         MVI   ERRFLG,X'02'        INVALID MKT OVERIDE SOURCE                   
*                                                                               
         CLI   CNCT,X'80'          IF WE WERE NOT CNCTD,WE NEVER SWTCHD         
         BO    DATAV22                                                          
         L     RF,ACOMFACS         SWITCH BACK TO CNTL SYSTEM                   
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,0                                                         
         BE    DATAV22                                                          
         XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(19),=C'SYSTEM SWITCH ERROR'                               
         B     EXIT                                                             
*                                  ADD/WRITE RECORD & EXIT                      
DATAV22  CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         MVI   FERN,X'FE'                                                       
*                                  ADD/WRITE RECORD & EXIT                      
DATAV30 LA     R1,DEMSRCH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
*                                                                               
         CLI   PFKEY,1             INFO LIST REQUESTED?                         
         BNE   DATAV35             NO                                           
         MVC   LKEY-LFMSAVE(L'LKEY,R2),KEY                                      
*         BAS   RE,CALLSCR          YES, LOAD LST SCREEN                        
         GOTO1 VSUBR01,DMCB,('CALLSCE',(R9)),(RC)                               
         MVC   RMKMKID(3),=C'INF'                                               
         LA    RE,SAVESTR                                                       
*        MVC   SVTKEY-SVTD(25,RE),KEY                                           
         B     INFO                                                             
*                                                                               
DATAV35  CLI   PFKEY,2             WAS A MKT LISTING REQUESTED?                 
         BNE   DATAV55                                                          
         LA    RE,SAVESTR                                                       
         USING SVTD,RE                                                          
         LA    R1,NVMKT                                                         
         CLI   SVTLSTYP,CTRMTPV    VALID MKT LISTING?                           
         BE    DATAV40                                                          
         LA    R1,NOMKT            OVERIDE LISTING?                             
         CLI   SVTLSTYP,X'01'      WAS MKT OVR LIST REQ?                        
         BE    DATAV40             NO                                           
         LA    R1,NIMKT            INVALID LISTING                              
*                                                                               
DATAV40 ST     R1,AMKT                                                          
         CLI   0(R1),0             MAKE SURE LIST IS NOT EMPTY                  
         BZ    DATAV45                                                          
         MVC   SVTELLN,0(R1)       # MKTS IN LIST                               
         MVC   SVTMTYP,1(R1)       MKT TYPE                                     
         ZIC   R1,SVTELLN          COMPUTE LENGTH OF MKT LIST                   
         SLL   R1,1                R1=#BYTES TO MOVE                            
         LA    RF,SVTMKTS          DESTINATION                                  
         L     RE,AMKT             SOURCE                                       
         LA    RE,2(RE)                                                         
         MOVE  ((RF),(R1)),(RE)    COPY MKTS TO SAVED LIST                      
         DROP  RE                                                               
*                                                                               
DATAV45 TM     CNCT,X'80'+X'01'    MKT OVR BUT NOT CNTD TO AGY?                 
         BNO   DATAV50             YES, DON'T DO LISTING                        
         XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(35),=C'RECONNECT TO AGENCY FOR MKT LISTING'               
         OI    BASHDRH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
DATAV50 DS     0H                                                               
         MVC   LKEY-LFMSAVE(L'LKEY,R2),KEY                                      
*         BAS   RE,CALLSCR          SAVE THIS SCREEN, LOAD MKT LST SCRN         
         GOTO1 VSUBR01,DMCB,('CALLSCE',(R9)),(RC)                               
         LA    RE,SAVESTR                                                       
         CLI   SVTLSTYP-SVTD(RE),X'01'    MKT OVR LIST?                         
         BE    SPLST00                                                          
         B     MKDSP00             NO, LIST FROM DEMAND                         
*                                                                               
*--IF ERRFLG=0 AND ENTER KEY HIT, BUILD MKT ELEMENTS                            
DATAV55 DS     0H                                                               
         CLI   ERRFLG,0                                                         
         BE    DATAV60             PROCESS ERRORS- MOVE IN ERR MSG              
         MVI   FERN,X'FE'                                                       
         OI    BASHDRH+6,X'80'                                                  
         LA    R6,DEMIMARH                                                      
         ST    R6,FADR                                                          
         CLI   ERRFLG,X'01'        DUPLICATE MKT                                
         BE    XIT                                                              
         CLI   ERRFLG,X'02'        INVALID MKT OVR SOURCE?                      
         BNE   XIT                                                              
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(30),=C'MISSING/INVALID MKT OVR SOURCE'                    
         LA    R6,DEMOMARH                                                      
         ST    R6,FADR                                                          
         B     XIT                                                              
*                                                                               
DATAV60 LA     R1,NIMKT            BUILD INVALID MKT ELEMENT                    
         ST    R1,AMKT                                                          
*         BAS   RE,BLDELMT                                                      
         GOTO1 VSUBR01,DMCB,('BLDELME',(R9)),(RC)                               
         LA    R1,NVMKT            BUILD VALID MKT ELEMENT                      
         ST    R1,AMKT                                                          
*         BAS   RE,BLDELMT                                                      
         GOTO1 VSUBR01,DMCB,('BLDELME',(R9)),(RC)                               
         LA    R1,NOMKT            BUILD OVERIDE MKT ELMNT                      
         ST    R1,AMKT                                                          
*         BAS   RE,BLDELMT                                                      
         GOTO1 VSUBR01,DMCB,('BLDELME',(R9)),(RC)                               
*                                                                               
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* INFO - LIST THE KEYS OF ALL DCON RECDS THAT HAVE SAME AGY/SRC/MEDIA           
*        ACTIVATED BY PF1.  RETURN TO MAIN SCREEN WITH PF12                     
**********************************************************************          
*                                                                               
INFO     DS    0H                                                               
         MVC   RMKMKID(3),=C'INF'                                               
         CLI   PFKEY,12                                                         
         BE    RESTR               RESTORE MAIN SCREEN                          
*INFO40   GOTO1 =A(INFOLST),DMCB,(R2),(R9),(RC),RR=RELO                         
INFO40   GOTO1 VSUBR02,DMCB,('INFOLSE',(R9)),(RC)                               
         CLI   DMCB,X'01'          GO TO DISPLAY?                               
         BNE   INFOX                                                            
         LR    R1,R2                                                            
         USING LFMSAVE,R1                                                       
         XC    LKEY,LKEY                                                        
         MVI   LACTN,DISPLAY                                                    
         XC    LNEXT,LNEXT                                                      
         MVC   BASACTN(7),=C'DISPLAY'                                           
         OI    BASACTNH+6,X'80'                                                 
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(16),=C'RECORD DISPLAYED'                                  
         OI    BASHDRH+6,X'80'                                                  
         MVI   FERN,X'FE'                                                       
         MVI   ACTN,DISPLAY                                                     
         B     DISPREC                                                          
*                                                                               
INFOX    B     XIT                                                              
         DROP  R1                                                               
*                                                                               
         SPACE 3                                                                
**********************************************************************          
* MKTDSP  -    LIST MKTS FOR THIS RECORD                                        
**********************************************************************          
*                                                                               
MKTDSP   DS    0H                  ENTER HERE WHEN SCRN WAS ALRDY UP            
         MVC   RMKMKID(3),=C'MKT'                                               
         LA    R5,SAVESTR          PT TO SAVED STORAGE                          
         USING SVTD,R5                                                          
         MVC   ACTN,SVTACT         RESTORE ACTION TYPE REQUESTED                
         MVI   ERRFLG,0            RESET ERROR FLAG                             
*                                                                               
         CLI   ACTN,DISPLAY                                                     
         BE    MKDSPA                                                           
*         BAS   RE,SELMKT          PICK OFF SELECTED MKTS AND SAVE AWAY         
         GOTO1 VSUBR01,DMCB,('SELMKE',(R9)),(RC)                                
MKDSPA   CLI   PFKEY,12            RETURN TO MAIN SCREEN?                       
         BE    RESTR               RESTORE ORIGINAL SCREEN                      
*                                                                               
MKDSP00  LA    R5,SAVESTR                                                       
         MVC   RMKMKID(3),=C'MKT'                                               
         MVC   RMKLBL(15),=C'  VALID LISTING'                                   
         CLI   SVTLSTYP,CTRMTPI                                                 
         BNE   *+10                                                             
         MVC   RMKLBL(2),=C'IN'                                                 
         OI    RMKLBLH+6,X'80'                                                  
         MVC   ACTN,SVTACT         RESTORE ACTION TYPE REQUESTED                
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
MKDSP10  MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,SVTAGY                                                  
******   MVC   DBSELAGY,SVAPHAGY                                                
         MVC   DBAREC,AMYIO          ADDRESS OF IO AREA                         
         MVC   DBSELSRC,SVTSRC                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,SVTMED                                                  
*        MVC   DMCB(1),SVTMED         PASS MEDIA CODE TO BLDMKT                 
*         BAS   RE,BLDMKT           BUILD LIST OF MARKETS                       
         CLI   USIDFLAG,USIDBIN                                                 
         BNE   MKDSP10A                                                         
         LA    RE,DBXTUID                                                       
         MVC   DBXTUID+4(4),DBEXTEND                                            
         STCM  RE,15,DBEXTEND                                                   
         USING DBXUIID,RE                                                       
         MVC   DBXUIID,=C'UID '                                                 
         MVC   DBXUUID,SVBINAGY                                                 
         DROP  RE                                                               
MKDSP10A GOTO1 VSUBR01,DMCB,('BLDMKE',(R9)),(RC)                                
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         OC    NMKTS,NMKTS         DOES LIST HAVE ANY MARKETS?                  
         BZ    MKDSP11             NO                                           
*         BAS   RE,MKBFILL          DISPLAY MKTS ON SCREEN                      
         GOTO1 VSUBR01,DMCB,('MKBFILE',(R9)),(RC)                               
*                                                                               
MKDSP11  XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         OI    BASHDRH+6,X'80'                                                  
         CLI   ERRFLG,X'01'        DUPLICATE MARKET?                            
         BNE   MKDSP15                                                          
         MVC   BASHDR(15),=C'DUPLICATE MKT : '                                  
         LA    RF,BASHDR+15                                                     
         EDIT  (2,ERRMKT),(4,0(RF)),ALIGN=LEFT                                  
         B     MKDSPX                                                           
*                                                                               
MKDSP15  MVC   BASHDR(21),=C'MARKET LIST DISPLAYED'                             
         CLI   SVTACT,DISPLAY                                                   
         BE    MKDSPX                                                           
         MVC   BASHDR(21),=C'SELECT/CHANGE MARKETS'                             
         MVI   ACTN,DISPLAY                                                     
*                                                                               
MKDSPX   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* ********************************************************************          
* RESTR- RESTORE MAIN SCREEN. TRANSFER THE SELECTED LIST TO THE NEW             
*        SCREEN                                                                 
* ********************************************************************          
RESTR    DS    0H                                                               
         CLC   RMKMKID(3),=C'INF'                                               
         BNE   *+8                                                              
         MVI   PFKEY,1                                                          
*                                                                               
         MVI   TWANUM,2            RESTORE OLD SCREEN                           
         MVC   COMAND2,=C'DMREAD  '                                             
*         BAS   RE,TWAIO                                                        
         GOTO1 VSUBR01,DMCB,('TWAIE',(R9)),(RC)                                 
         LA    R4,BASKEYH                                                       
RESTR5   ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   RESTR5                                                           
         MVC   1(2,R4),=X'0101'                                                 
         LA    R4,DEMSRCH                                                       
         OI    6(R4),X'01'                                                      
*                                                                               
         LA    RE,SAVESTR                                                       
         USING SVTD,RE                                                          
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(16),=C'RECORD DISPLAYED'                                  
         TM    SVTRSTAT,X'80'      TEST IF RECORD DELETED                       
         BZ    *+10                                                             
         MVC   BASHDR+16(11),=C'-IS DELETED'                                    
         CLI   SVTACT,DISPLAY      IF ACTN=DISP, DON'T REBUILD LIST             
         BE    RESTRX                                                           
         MVC   BASHDR(32),=C'MAKE CHANGES OR ENTER TO PROCESS'                  
*                                                                               
         CLI   PFKEY,1             WAS THIS AN INFO LIST?                       
         BE    RESTRX                                                           
*                                                                               
         CLI   SVTLSTYP,CTRMTPV                                                 
         BNE   RESTR10                                                          
         TWAXC DEMVMARH,DEMVMAXH                                                
         B     RESTR20                                                          
*                                                                               
RESTR10  CLI   SVTLSTYP,CTRMTPI                                                 
         BNE   RESTR15                                                          
         TWAXC DEMIMARH,DEMIMAXH                                                
         B     RESTR20                                                          
*                                                                               
RESTR15  TWAXC DEMOMARH,DEMOMAXH                                                
*                                                                               
RESTR20  LA    RE,SAVESTR                                                       
         LA    R1,NVMKT                                                         
         CLI   SVTLSTYP,CTRMTPV    VALID MKT ELMNT?                             
         BE    RESTR22                                                          
         LA    R1,NIMKT                                                         
         CLI   SVTLSTYP,CTRMTPI    INVALID MKT ELM?                             
         BE    RESTR22                                                          
         LA    R1,NOMKT            OVERIDE MKT                                  
         MVC   OTYPE,SVTMTYP       SAVE SOURCE                                  
         CLC   OTYPE,SVTSRC        SOURCE CANNOT BE SAME AS RECD'S SRC          
         BNE   *+8                                                              
         MVI   OTYPE,C'?'          SET TO INVALID                               
         MVI   ERRFLG,X'02'        INVALID MKT SOURCE                           
*                                                                               
RESTR22  ST    R1,AMKT                                                          
         XC    2(250,R1),2(R1)     PARTIALLY CLEAR DESTIN                       
         ZIC   R7,SVTELLN                                                       
         STC   R7,0(R1)                                                         
         CLI   0(R1),0                                                          
         BZ    RESTR24             LIST IS EMPTY                                
*                                                                               
         LA    RE,SVTMKTS          COPY SVTMKTS TO LISTS AT SCRN BTM            
         LA    R1,2(R1)            PT TO 1ST MKT BUCKET DESINATION              
RESTR23  OC    0(2,RE),0(RE)       IS SOURCE MKT=0?                             
         BNZ   *+12                                                             
         LA    RE,2(RE)            YES,BUMP SRC BCKT & NOT COUNT (R7)           
         B     RESTR23                                                          
         MVC   0(2,R1),0(RE)       NO, SAVE MKT NUMBER IN LIST                  
         LA    R1,2(R1)                                                         
         LA    RE,2(RE)                                                         
         BCT   R7,RESTR23          COPY ALL MKTS                                
*                                                                               
         L     R5,AMKT                                                          
*         GOTO1 =A(DSPMKT),DMCB,(R2),(R3),(R9),(RC),RR=RELO                     
         GOTO1 VSUBR02,DMCB,('DSPMKE',(R9)),(RC)                                
*                                                                               
RESTR24  LA    RE,SAVESTR                                                       
         LA    R5,SVTEL04                                                       
         CLI   SVTELLN,0           NO MARKETS DEFINED                           
         BE    RESTRX                                                           
         CLI   SVTLSTYP,X'01'      WAS THIS A MKT OVR LISTING?                  
         BNE   RESTR25                                                          
         CLI   2(R5),0             WAS A SOURCE DEFINED?                        
         BNE   RESTR25                                                          
         MVI   2(R5),C' '          NO MOVE IN A BLANK                           
RESTR25  ZIC   R7,SVTELLN          # MKTS IN LIST                               
         L     RF,AREC             PUT IMPORTANT PART OF KEY BACK IN            
         MVC   CTRKSRC-CTRREC(CTRLEN-CTRKSRC,RF),SVTSRC                         
         DROP  RE                                                               
*                                                                               
RESTRX   LA    R4,BASKEYH                                                       
RESTRX1  ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   RESTRX1                                                          
         MVC   1(2,R4),=X'0101'                                                 
         LA    R4,DEMSRCH                                                       
         OI    6(R4),X'01'                                                      
*                                                                               
         LA    RE,SAVESTR          CLEAR 1ST PART OF SAVE STORE                 
         LH    RF,=Y(SVTRECQ)                                                   
         XCEF                                                                   
*                                                                               
         MVI   FERN,X'FE'          THIS PHASE SUPPLIES FLDH MSG                 
         MVI   LACTN-LFMSAVE(R2),DISPLAY                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SPLST   -    SPOT LIST FOR MARKET OVERIDE DISPLAY                             
**********************************************************************          
*                                                                               
SPLST    DS    0H                                                               
         MVC   RMKMKID(3),=C'SPT'  CURRENT SCRN IS MKT OVR LISTING              
         MVI   ERRFLG,0                                                         
         LA    R5,SAVESTR          PT TO SAVED STORAGE                          
         USING SVTD,R5                                                          
         MVC   ACTN,SVTACT         RESTORE ACTION TYPE REQUESTED                
*                                                                               
         CLI   ACTN,DISPLAY                                                     
         BE    SPLST1C                                                          
*                                  ACT=CHANGE - VALIDATE SOURCE FIELD           
         CLI   RMKSRCH+5,0         ANY INPUT?                                   
         BNE   *+12                                                             
         MVI   SVTMTYP,0           FIELD IS EMPTY                               
         B     SPLST1B                                                          
*                                                                               
         ZIC   RF,RMKSRCH+5                                                     
         BCTR  RF,0                RF=L'INPUT FIELD-1                           
         L     RE,=A(SRCTAB)       RE=A(SOURCE TABLE)                           
         A     RE,RELO                                                          
SPLST1   CLI   0(RE),0             TEST E-O-T                                   
         BNE   *+12                                                             
         MVI   SVTMTYP,0           INVALID INPUT                                
         B     SPLST1B                                                          
         EXCLC RF,0(RE),RMKSRC     VALID SOURCE?                                
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     SPLST1                                                           
         MVC   SVTMTYP,8(RE)       SET NEW SOURCE CODE                          
*                                                                               
*SPLST1B  BAS   RE,SELMKT          PICK OFF SELECTED MKTS AND SAVE AWAY         
SPLST1B  GOTO1 VSUBR01,DMCB,('SELMKE',(R9)),(RC)                                
*                                                                               
SPLST1C  CLI   PFKEY,12            RETURN TO MAIN SCREEN?                       
         BE    RESTR               RESTORE ORIGINAL SCREEN                      
*                                                                               
SPLST00  DS    0H                  COME IN HERE IF 1ST TIME DISP                
         LA    R5,SAVESTR          PT TO SAVED STORAGE                          
         MVI   ERRFLG,0                                                         
         MVC   RMKMKID(3),=C'SPT'                                               
         MVC   RMKLBL(23),=C'MARKET OVERRIDE LISTING'                           
         MVC   RMKSRCL(7),=C'SOURCE='                                           
*                                                                               
         XC    RMKSRC,RMKSRC                                                    
         L     RE,=A(SRCTAB)       RE=A(SOURCE TABLE)                           
         A     RE,RELO                                                          
SPLST01  CLI   0(RE),0                                                          
         BNE   SPLST02                                                          
         MVC   RMKSRC(3),=C' ? '   SET INVALID SOURCE TO ?                      
         MVI   ERRFLG,X'02'                                                     
         B     SPLST02A                                                         
*                                                                               
SPLST02  CLC   L'SRCTAB-1(1,RE),SVTMTYP                                         
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     SPLST01                                                          
         MVC   RMKSRC(8),0(RE)       FIRST OUTPUT ENTRY IS SOURCE               
*                                                                               
SPLST02A OI    RMKLBLH+6,X'80'     TRANSMIT -LABEL                              
         OI    RMKSRCLH+6,X'80'             -SOURCE LABEL                       
         OI    RMKSRCH+6,X'80'              -SOURCE                             
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0   CONNECT TO CORRECT SPT SYSTEM              
         CLI   DMCB+4,0                                                         
         BE    SPLST10                                                          
         CLI   DMCB+4,2                                                         
         BNE   SPLST03                                                          
         GOTO1 (RF),DMCB,=C'CON',0   RECONNECT TO CNTRL SYSTEM                  
SPLST03  XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(19),=C'SYSTEM SWITCH ERROR'                               
         B     EXIT                                                             
*                                                                               
SPLST10  DS    0H                  READ MKT RECORDS                             
         XC    BASHDR,BASHDR                                                    
         OI    BASHDRH+6,X'80'                                                  
         MVI   FERN,X'FE'                                                       
         CLI   ACTN,DISPLAY                                                     
         BNE   SPLST20                                                          
*         BAS   RE,SPDSP            IF DISPLAY, JUST DISP MKTS IN LIST          
         GOTO1 VSUBR01,DMCB,('SPDSE',(R9)),(RC)                                 
         MVC   BASHDR(21),=C'MARKET LIST DISPLAYED'                             
         B     SPLSTX                                                           
*                                                                               
SPLST20  DS    0H                                                               
*         BAS   RE,SPCHG            FOR CHANGE, DISP ALL MKTS                   
         GOTO1 VSUBR01,DMCB,('SPCHE',(R9)),(RC)                                 
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(21),=C'SELECT/CHANGE MARKETS'                             
*                                                                               
SPLSTX   CLI   ERRFLG,X'02'        INVALID MKT OVR SOURCE?                      
         BNE   *+16                                                             
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(30),=C'MISSING/INVALID MKT OVR SOURCE'                    
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*============================= CTLFMERRS =============================*         
       ++INCLUDE CTLFMERRS                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== LITERARY POOL ===========================*         
         LTORG                                                                  
         DROP  R2,R9,RA,RB,RC                                                   
***********************************************************************         
         TITLE 'CTLFM1A <==> TA021A - DEMO CONTROL RECORDS : SUBR01'            
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
SUBR01Q  EQU   ((((*-CTLFM1A)/4096)+1)*4096)                                    
         ORG   CTLFM1A+SUBR01Q                                                  
SUBR01   CSECT                                                                  
         NMOD1 0,**SR01**,RR=RA                                                 
         L     R9,0(R1)                                                         
         USING LFMTEMP,R9          R9=A(GLOBAL TEMP W/S)                        
         L     RC,4(R1)                                                         
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         L     R2,ATWA                                                          
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         ST    RA,RELO1            RELOCATION FACTOR                            
         L     R1,0(R1)                                                         
         SRL   R1,24                                                            
         SLL   R1,2                                                             
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
         SPACE 1                                                                
DTTOWE   EQU   (DTTOW#-*)/4+1                                                   
STRMKE   EQU   (STRMK#-*)/4+1                                                   
BLDELME  EQU   (BLDELM#-*)/4+1                                                  
VALDUE   EQU   (VALDU#-*)/4+1                                                   
GETMKTE  EQU   (GETMKT#-*)/4+1                                                  
BLDMKE   EQU   (BLDMK#-*)/4+1                                                   
MKBFILE  EQU   (MKBFIL#-*)/4+1                                                  
ELEMLSE  EQU   (ELEMLS#-*)/4+1                                                  
SELMKE   EQU   (SELMK#-*)/4+1                                                   
SPDSE    EQU   (SPDS#-*)/4+1                                                    
SPCHE    EQU   (SPCH#-*)/4+1                                                    
CALLSCE  EQU   (CALLSC#-*)/4+1                                                  
TWAIE    EQU   (TWAI#-*)/4+1                                                    
         SPACE 1                                                                
DTTOW#   B     DTTOWK                                                           
STRMK#   B     STRMKT                                                           
BLDELM#  B     BLDELMT                                                          
VALDU#   B     VALDUP                                                           
GETMKT#  B     GETMKTS                                                          
BLDMK#   B     BLDMKT                                                           
MKBFIL#  B     MKBFILL                                                          
ELEMLS#  B     ELEMLST                                                          
SELMK#   B     SELMKT                                                           
SPDS#    B     SPDSP                                                            
SPCH#    B     SPCHG                                                            
CALLSC#  B     CALLSCR                                                          
TWAI#    B     TWAIO                                                            
         EJECT                                                                  
*====================== DATE TO WEEK CONVERSION ======================*         
*        AT ENTRY,                                                              
*          DUB       = DATE                                                     
*          WORK(2)   = SOURCE/MEDIA                                             
*          WORK+2(1) = L(INPUT DATE)                                            
*        AT EXIT,                                                               
*          CC = 0 IF VALID                                                      
*             <>0 IF INVALID                                                    
*          MYHALF =  CORRESPONDING BOOK DATE, IF VALID                          
*                                                                               
DTTOWK   DS    0H                                                               
         L     RE,=A(SMTAB)                                                     
         A     RE,RELO1                                                         
DTW10    CLI   0(RE),0             IF AT END OF TABLE,                          
         BNE   *+6                                                              
         DC    H'0'                 DIE!                                        
         CLC   0(2,RE),WORK                                                     
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     DTW10                                                            
         ICM   R1,15,(SMADW-SMTAB)(RE)                                          
         BZ    *+8                                                              
         A     R1,RELO1                                                         
         ST    R1,ADTWK            GET A(DATE TO WEEK CONVRSN)                  
         SPACE 1                                                                
         MVI   DUB2,0              SET INPUT BITS                               
         OI    DUB2,PVINSGLO        SINGLE DATE ONLY IS VALID                   
         OI    DUB2,PVINSGLS        SINGLE DATE RETURNED AS SINGLE              
         OI    DUB2,X'01'           ENGLISH LANGUAGE                            
         GOTO1 PERVAL,DMCB,(WORK+2,DUB),(DUB2,PERVOUT)                          
         TM    4(R1),PVRCINV1      DATE ONE INVALID?                            
         BO    DTWNO                YEP, ERROR                                  
         SPACE 1                                                                
         LA    RE,PERVOUT          RE-->PERVAL OUTPUT BLOCK                     
         USING PERVALD,RE                                                       
         TM    PVALASSM,PVALASM+PVALASY    ASSUMED MONTH OR YEAR?               
         BNZ   DTWNO                        YEP, ERROR                          
         OC    ADTWK,ADTWK                                                      
         BNZ   DTW15                                                            
         TM    PVALASSM,PVALASD    VALIDATE FOR M/Y                             
         BZ    DTWNO                SO EXIT IF DAY NOT ASSUMED                  
         B     DTW15A                                                           
DTW15    TM    PVALASSM,PVALASD    VALIDATE FOR M/D/Y                           
         BO    DTWNO                SO EXIT IF DAY ASSUMED                      
         B     DTW15A                                                           
DTW15A   MVC   MYDATE,PVALESTA     GET YYMMDD                                   
         DROP  RE                                                               
         SPACE 1                                                                
         OC    ADTWK,ADTWK                                                      
         BNZ   DTW20               NEED TO VALIDATE SPECIALLY                   
*                                                                               
* MMMYY BOOK                                                                    
*                                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(0,MYDATE),(3,MYHALF)                               
*                                                                               
*        PACK  DUB2,MYDATE(2)      YEAR                                         
*        CVB   R1,DUB2                                                          
*        STC   R1,MYHALF                                                        
*        PACK  DUB2,MYDATE+2(2)    MONTH                                        
*        CVB   R1,DUB2                                                          
*        STC   R1,MYHALF+1                                                      
*                                                                               
         B     DTWYES              EXIT WITH GOOD CC                            
*                                                                               
* MMMDD/YY BOOK                                                                 
*                                                                               
DTW20    DS    0H                                                               
         L     R0,=V(NSIWEEK)                                                   
         A     R0,RELO1                                                         
         C     R0,ADTWK            IF ADTWK POINTS TO NSIWEEK,                  
         BNE   *+10                                                             
         MVC   DMCB+12(4),VDATCON   SET 4TH PARM TO A(DATCON)                   
         GOTO1 ADTWK,DMCB,MYDATE,GETDAY,ADDAY                                   
         MVC   MYHALF(1),4(R1)     YEAR                                         
         MVC   MYHALF+1(1),0(R1)   WEEK NO.                                     
         B     DTWYES                                                           
*                                                                               
DTWNO    LA    RF,1                                                             
         B     *+6                                                              
DTWYES   SR    RF,RF                                                            
         LTR   RF,RF               RETURN WITH CONDITION CODE SET               
         B     EXITS1                                                           
         EJECT                                                                  
*=============== STRMKT-COPY MKTS TO SAVED STORAGE LIST ==============*         
*                                                                               
*       R5 -  PTS TO ELEMENT TO COPY                                            
*                                                                               
STRMKT   DS    0H                                                               
         LA    R1,NVMKT                                                         
         CLI   2(R5),CTRMTPV       VALID MKT ELMNT?                             
         BE    STRMK5                                                           
         LA    R1,NIMKT                                                         
         CLI   2(R5),CTRMTPI       INVALID MKT ELM?                             
         BE    STRMK5                                                           
         LA    R1,NOMKT            OVERIDE MKT                                  
*                                                                               
STRMK5   ST    R1,AMKT                                                          
         MVC   1(1,R1),2(R5)       SAVE MKT TYPE                                
         ZIC   RF,0(R1)            #MKTS IN LIST ALREADY                        
         SLL   RF,1                2BYTES FOR EACH MKT                          
         L     RE,AMKT                                                          
         LA    RE,2(RE)            ADD WHERE TO COPY MKTS TO                    
         AR    RE,RF                                                            
         ZIC   R1,1(R5)            LENGTH OF ELEMENT                            
         SH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),3(R5)      MOVE IN MKTS                                 
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         L     RE,AMKT                                                          
         ZIC   R0,0(RE)                                                         
         AR    R1,R0               TOTAL # MKTS                                 
         STC   R1,0(RE)                                                         
*                                                                               
STRSORT  DS    0H                                                               
         L     R1,AMKT                                                          
         CLI   0(R1),1                                                          
         BNH   STRMKTX                                                          
         ZIC   R7,0(R1)            #MKTS IN LIST                                
         LA    R8,2(R1)            1ST MKT IN LIST                              
         L     RF,=V(XSORT)                                                     
         A     RF,RELO1                                                         
         GOTO1 (RF),DMCB,(R8),(R7),2,2,0                                        
*                                                                               
STRMKTX  B     EXITS1                                                           
         EJECT                                                                  
*============ BLDELMT- BUILD ELEMENTS FROM LIST IN SAVESTR ===========*         
*                                                                               
*         AMKT = A(LIST) TO BUILD ELEMENT FROM)                                 
*                                                                               
BLDELMT  DS    0H                                                               
         L     R5,AMKT                                                          
         CLI   0(R5),0             HOW MANY MKTS IN LIST?                       
         BZ    BLDELX              NONE- EXIT                                   
         MVI   FLAG,0              #MKTS READ SO FAR FROM SRC LIST              
*                                                                               
BLDEL05  XC    TEMP,TEMP           SAVE MKTS IN TEMP                            
         MVI   TEMP,CTRMECDQ       MKT ELEMENT CODE                             
         MVI   TEMP+1,3                                                         
         L     R1,AMKT                                                          
         MVC   TEMP+2(1),1(R1)     SAVE MKTTYPE                                 
         LA    R8,TEMP+3           DESTIN OF MKTS IN TEMP                       
         CLI   FLAG,0              1ST TIME ENTRY?                              
         BNE   *+8                 NO, R5 ALREADY PTS TO NEXT SRC MKT           
         LA    R5,2(R1)            SOURCE LIST MKTS                             
*                                                                               
BLDEL10  L     R1,AMKT                                                          
         CLC   FLAG,0(R1)          HAVE WE READ ALL SRC MKTS?                   
         BE    BLDEL20             YES, ADD ELEMENT                             
*                                                                               
         MVC   0(2,R8),0(R5)       MOVE IN MKT TO TEMP                          
         ZIC   R1,FLAG                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FLAG             TOTAL #MKTS PROCESSED SO FAR                 
         ZIC   R1,TEMP+1                                                        
         LA    R1,2(R1)                                                         
         STC   R1,TEMP+1           LENGTH OF ELEMENT                            
         CLI   TEMP+1,253          MAX 125 MKTS + 3OVERHEAD = 253               
         BNL   BLDEL20             DONE WITH THIS ELEMENT                       
         LA    R8,2(R8)            BUMP ELEMENT PRT                             
         LA    R5,2(R5)            BUMP SOURCE  PTR                             
         B     BLDEL10                                                          
*                                                                               
BLDEL20  CLI   TEMP+1,3            MUST HAVE MKTS TO SAVE ELEMENT               
         BNH   BLDELX                                                           
         GOTO1 APUTEL              SAVE ELEMENT TO RECORD                       
         BZ    BLDELX              ERROR, EXIT                                  
         L     R1,AMKT                                                          
         CLC   FLAG,0(R1)          HAVE WE READ ALL SRC MKTS?                   
         BL    BLDEL05             NO, BUILD ANOTHER ELEMENT                    
*                                                                               
BLDELX   B     EXITS1              ELEMENT(S) BUILT                             
         EJECT                                                                  
*== VALDUP - SEARCH FOR DUPLICATE MKTS BETWEEN VALID AND INVALID MKT =*         
*================ LIST IN SAVED STORAGE: VMKTS - IMKTS ===============*         
*                                                                               
VALDUP   DS    0H                                                               
         CLI   NIMKT,0             ARE THERE ANY INVALID MKTS?                  
         BZ    VALDPX              NO DUPLICATES                                
         CLI   NVMKT,0             ARE THERE ANY VALID MKTS                     
         BZ    VALDPX              NO DUPLICATES                                
         LA    RE,IMKTS            STEP THRU INVALID LIST                       
         ZIC   R0,NIMKT                                                         
*                                                                               
VALDP5   LA    RF,VMKTS                                                         
         ZIC   R1,NVMKT                                                         
         CLC   0(2,RF),0(RE)       SAME MKT IN BOTH LISTS?                      
         BE    VALDP10                                                          
         LA    RF,2(RF)            NO, NEXT VALID MKT                           
         BCT   R1,*-14                                                          
*                                                                               
         LA    RE,2(RE)            NEXT INV MKT                                 
         BCT   R0,VALDP5                                                        
         B     VALDPX              NO DUPLICATES                                
*                                                                               
VALDP10  MVI   ERRFLG,X'01'        SET ERROR FLG TO DUPS FND                    
         MVC   ERRMKT,0(RE)        SAVE DUPL MKT #                              
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(15),=C'DUPLICATE MKT : '                                  
         LA    RF,BASHDR+15                                                     
         EDIT  (2,0(RE)),(4,0(RF)),ALIGN=LEFT                                   
*                                                                               
VALDPX   B     EXITS1                                                           
         EJECT                                                                  
*========== GETMKTS - READ MKTS FROM DEMAND AND SAVE IN BUFF =========*         
*                                                                               
GETMKTS  DS    0H                                                               
         XC    DBLOCK,DBLOCK                                                    
         XC    NMKTS,NMKTS                                                      
         LA    R5,KEY                                                           
         USING CTRREC,R5                                                        
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
******   MVC   DBSELAGY,CTRKAGY                                                 
         MVC   DBSELAGY,SVAPHAGY                                                
         MVC   DBAREC,AMYIO            ADDRESS OF IO AREA                       
         MVC   DBSELSRC,CTRKSRC                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CTRKMED                                                 
         CLI   USIDFLAG,USIDBIN                                                 
         BNE   GETMKT30                                                         
         LA    RE,DBXTUID                                                       
         MVC   DBXTUID+4(4),DBEXTEND                                            
         STCM  RE,15,DBEXTEND                                                   
         USING DBXUIID,RE                                                       
         MVC   DBXUIID,=C'UID '                                                 
         MVC   DBXUUID,CTRKAGY                                                  
         DROP  RE                                                               
GETMKT30 DS    0H                      BUILD LIST OF MARKETS IN 'BUFF'          
         GOTO1 VSUBR01,DMCB,('BLDMKE',(R9)),(RC)                                
         CLI   FERN,X'FF'                                                       
         MVI   DMCB,X'01'           UNABLE TO READ MKTS                         
*                                                                               
         MVI   FERN,X'FF'                                                       
         B     EXITS1                                                           
         SPACE 1                                                                
         DROP  R5                                                               
         EJECT                                                                  
*==================== BLDMKT - BUILD MARKET LIST =====================*         
*                                                                               
*              DMCB(1) = MEDIA                                                  
*                                                                               
BLDMKT   DS    0H                                                               
         L     RE,ABUFF                                                         
         LR    R3,RE                                                            
         L     RF,=F'12000'                                                     
         XCEF                                                                   
         CLI   DBSELMED,C'N'       FOR NETWORK,MOVE IN MKTS FROM NETTBL         
         BE    BLDM10                                                           
*                                                                               
         L     RF,ACOMFACS         SWITCH TO SPOT SYSTEM                        
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,0                                                         
         BE    BLDM5                                                            
         CLI   DMCB+4,2                                                         
         BNE   BLDM2                                                            
         GOTO1 (RF),DMCB,=C'CON',0                                              
BLDM2    XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(19),=C'SYSTEM SWITCH ERROR'                               
         OI    BASHDRH+6,X'80'                                                  
         B     EXITS1                                                           
*                                                                               
BLDM5    MVI   DBFUNCT,DBGETMKN                                                 
         L     R3,ABUFF                                                         
         ST    R3,FULL                                                          
         XC    DUB,DUB                                                          
         GOTO1 DEMAND,DMCB,DBLOCK,MKBHOOK                                       
         L     R3,DUB                                                           
         ST    R3,NMKTS            SAVE # MKTS IN BUFFER                        
*         GOTO1 =A(UNKMKTS),DMCB,(R2),(R3),(R9),(RC),RR=RELO                    
         GOTO1 VSUBR02,DMCB,('UNKMKTE',(R9)),(RC)                               
*                                                                               
         L     RF,ACOMFACS         SWITCH BACK TO CONTROL SYSTEM                
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,0                                                         
         BE    BLDMX                                                            
         DC    H'0'                SYSTEM NOT OPEN                              
         SPACE 2                                                                
*                                                                               
BLDM10   DS    0H                  NETWORK ONLY                                 
         L     R3,ABUFF            FILL BUFFER WITH NETWORK MKTS                
         L     R1,=A(NETTBL)                                                    
         A     R1,RELO                                                          
         SR    R6,R6               CNT # ENTRIES IN NETTBL                      
*                                                                               
BLDM15   CLI   0(R1),X'FF'         END OF NETWORK TABLE?                        
         BE    BLDM20                                                           
         MVC   0(L'NETTBL,R3),0(R1)  MOVE IN MKT # AND NAME                     
         LA    R6,1(R6)            INCR MKT CNT IN BUFF                         
         LA    R3,26(R3)           BUMP 26 TO KEEP FMT LIKE DMD MKBHOOK         
         LA    R1,L'NETTBL(R1)     NEXT NETWORK TABLE ENTRY                     
         B     BLDM15                                                           
*                                                                               
BLDM20   ST    R6,NMKTS            SAVE TOT # MKTS IN BUFF                      
*                                                                               
BLDMX    B     EXITS1                                                           
         SPACE 2                                                                
*------------- MKBHOOK - HOOK FOR READING IN ALL MARKETS -------------*         
*                                                                               
MKBHOOK  NTR1                                                                   
         L     R3,FULL                                                          
         L     R4,DBAREC                                                        
         USING DMKEY,R4                                                         
         MVC   0(2,R3),DMRMKT                                                   
         LA    R4,DMFRSTEL                                                      
         DROP  R4                                                               
         USING DMELEM,R4                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         CH    R1,=Y(L'RMKMKA1)                                                 
         BL    *+8                                                              
         LA    R1,11                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),DMMNAME                                                  
         L     R1,DUB                                                           
         LA    R1,1(R1)                                                         
         ST    R1,DUB                                                           
         LA    R3,26(R3)                                                        
         ST    R3,FULL                                                          
         DROP  R4                                                               
         B     EXITS1                                                           
         EJECT                                                                  
*============== MKBFILL - PRINT MARKTETS ON THE SCREEN ===============*         
MKBFILL  DS    0H                                                               
         L     R3,NMKTS                                                         
         L     RF,=V(XSORT)                                                     
         A     RF,RELO                                                          
         XC    DMCB,DMCB                                                        
         L     R1,ABUFF                                                         
         ST    R1,DMCB                                                          
         GOTO1 (RF),DMCB,,(R3),26,12,2                                          
         LA    R3,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         TWAXC RMKMK1H,(R3),PROT=Y                                              
         L     R8,ABUFF            PT TO LIST OF MARKETS FROM DEMAND            
         LA    R3,RMKMK1H          1ST OUTPUT FIELD ON SCREEN (SELECT)          
         LA    R4,SAVESTR          LIST OF MKTS SPEC FOR THIS RECD              
         USING SVTD,R4                                                          
*                                                                               
MKBFILL2 LA    R1,RMKMKLH                                                       
         CR    R3,R1                                                            
         BH    EXITS1                                                           
         CLC   SVTMKT,2(R8)                                                     
         BNL   MKBFILL3                                                         
*                                                                               
         MVC   FNDMKT,0(R8)                                                     
         MVC   REPL,FNDMKT         IF FOUND, NO REPLCMENT NECC                  
*         BAS   RE,ELEMLST          SEE IF MKT IS IN SVMKT LIST                 
         GOTO1 VSUBR01,DMCB,('ELEMLSE',(R9)),(RC)                               
         CLI   SVTACT,DISPLAY                                                   
         BNE   *+16                                                             
         CLI   MARK,C'X'           IF DISPLY, ONLY DISP WHAT'S IN LIST          
         BNE   MKBFILL3            NOT IN RECORD, DON'T DISPLAY                 
         MVI   MARK,C' '           IN RECD- DON'T NEED TO PRINT THE X           
*                                                                               
         MVC   SVTMKT,2(R8)        SAVE MARKET NAME                             
         MVC   8(1,R3),MARK        PUT IN X IF NECCESARY (ELSE ' ')             
         OI    6(R3),X'80'         TRANSMIT UNPROT FIELD                        
         LA    R3,RMKMKA1H-RMKMK1H(R3)  PT TO MKT #-NAME FIELD                  
*                                                                               
         EDIT  (B2,0(R8)),(4,DUB)  CONVERT MKT # TO CHAR                        
         MVC   8(4,R3),DUB                DISPLAY MKT NUMBER                    
         MVC   8+5(L'RMKMKA1-5,R3),2(R8)  DISPLAY MKT NAME                      
         OI    6(R3),X'80'                                                      
         LA    R3,RMKMK2H-RMKMKA1H(R3) NEXT UNPROT SELECT FIELD                 
*                                                                               
MKBFILL3 LA    R8,26(R8)                                                        
         OC    0(2,R8),0(R8)                                                    
         BNZ   MKBFILL2                                                         
         XC    SVTMKT,SVTMKT           CLEAR LAST MKT READ                      
         DROP  R4                                                               
         B     EXITS1                                                           
         EJECT                                                                  
*=====================================================================*         
*ELEMLST-LOOK THRU MKT LIST IN THE ELEMENT TO SEE IF 0(R8) MKT IS THERE         
*        FNDMKT= MKT IN BUFFER TO COMPARE AGAINST                               
*        REPL= REPLACE MKT # IF REQUESTED MKT IS FOUND                          
*        NOTE: IF FNDMKT=0, PROC WILL SEARCH FOR EMPTY BUCKET & ADD             
*              THE MKT SPEC IN REPL.IF REPL=0 IT DELETES FNDMKT MARKET          
*                                                                               
ELEMLST  DS    0H                                                               
         LA    R4,SAVESTR                                                       
         USING SVTD,R4                                                          
         MVI   MARK,C' '           DEFAULT TO UNMARKED                          
         ZIC   R7,SVTELLN          R7- # MKTS IN LIST                           
         LA    R3,SVTMKTS                                                       
         CLI   SVTELLN,0                                                        
         BE    ELEM25              SEE IF REQ ACTN=ADD MKT                      
*                                                                               
ELEM10   CLC   0(2,R3),FNDMKT                                                   
         BNE   ELEM20              MKT NOT FOUND                                
         MVI   MARK,C'X'                                                        
         MVC   0(2,R3),REPL        MOVE IN REPLACE MKT #                        
*                                                                               
         OC    REPL,REPL           DELETE MKT?                                  
         BNZ   ELEM15                                                           
         CLI   SVTELLN,0           ADJUST MKT CNT-IF NON ZERO                   
         BZ    ELEMX                                                            
         ZIC   R1,SVTELLN                                                       
         BCTR  R1,0                                                             
         STC   R1,SVTELLN                                                       
         B     ELEMX                                                            
*                                                                               
ELEM15   OC    FNDMKT,FNDMKT       ADD MKT TO LIST?                             
         BNZ   ELEMX               NO                                           
         ZIC   R1,SVTELLN          ADJUST LENGTH                                
         LA    R1,1(R1)            INCR # MKTS IN LIST                          
         STC   R1,SVTELLN                                                       
         B     ELEMX                                                            
*                                                                               
ELEM20   OC    0(2,R3),0(R3)       IF EMPTY BUCKET,                             
         BNZ   *+12                JUST BRANCH, DON'T BUMP COUNTER              
         LA    R3,2(R3)            NEXT MKT IN LIST                             
         B     ELEM10              JUST BRANCH, DON'T BUMP COUNTER              
*                                                                               
         LA    R3,2(R3)            NEXT MKT IN LIST                             
         BCT   R7,ELEM10           MKT FOUND, BUMP MKT COUNTER                  
*                                                                               
ELEM25   OC    FNDMKT,FNDMKT       ADD MKT TO END OF LIST?                      
         BNZ   ELEMX               NO                                           
         CLI   SVTELLN,250         ALL BUCKETS FULL?  (MAX 2 ELEMNTS)           
         BNL   ELEMX               YES-CANNOT ADD MKT                           
         MVI   MARK,C'X'                                                        
         MVC   0(2,R3),REPL        ADD MKT TO END                               
         B     ELEM15              ADJUST #MKTS IN LIST COUNTER                 
*                                                                               
ELEMX    B     EXITS1                                                           
         DROP  R4                                                               
         EJECT                                                                  
*=========== SELMKT - SELECT A MARKET FROM MKT LIST SCREEN ===========*         
*                                                                               
SELMKT   DS    0H                                                               
*                                                                               
         LA    R3,RMKMK1H          1ST MKT ON SCREEN                            
*                                                                               
SELMKT2  LA    RF,RMKMKLH          LAST MKT ON SCREEN                           
         CR    R3,RF               HAVE WE REACHED BOTTOM OF SCREEN?            
         BH    SELMKTX             YES                                          
         CLI   5(R3),0             WAS THIS MKT SELECTED?                       
         BNE   SELMKT5             NO                                           
         ZIC   R0,0(R3)            NEXT FIELD (MKT FLD)                         
         AR    R3,R0                                                            
         B     SELMKT20                                                         
*                                                                               
SELMKT5  MVC   TEMP(1),8(R3)       SAVE CHAR INPUT                              
         ZIC   R0,0(R3)            YES, PT TO NXT FLD- CONTAINS MKT#            
         AR    R3,R0                                                            
         MVC   FULL,8(R3)          PICK OFF MKT # SELECTED                      
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         LTR   R1,R1               IGNORE IF NO MARKET                          
         BZ    SELMKT20                                                         
         STCM  R1,3,FNDMKT         SEARCH FOR MKT IN LIST                       
         MVC   REPL,FNDMKT                                                      
*                                                                               
         CLI   TEMP,C' '           DOES USER WANT TO ERASE THIS MKT?            
         BNE   *+10                NO                                           
         XC    REPL,REPL           ERASE MKT                                    
*                                                                               
*         BAS   RE,ELEMLST          SEE IF ALREADY IN SAVED MKT LIST            
         GOTO1 VSUBR01,DMCB,('ELEMLSE',(R9)),(RC)                               
         CLI   TEMP,C' '           WAS REQ=DELETE MKT?                          
         BE    SELMKT20            YES, IT WAS DONE OR NOT FOUND                
         CLI   MARK,C'X'           REQ=ADD MKT- WAS MKT FOUND?                  
         BE    SELMKT20            YES- PROCESS NEXT SEL FIELD                  
*                                                                               
         MVC   REPL,FNDMKT         SET REPL MKT TO MKT WE WANT TO ADD           
         XC    FNDMKT,FNDMKT       ADD MKT TO LST-FILL HOLES/ADD ON             
*         BAS   RE,ELEMLST                                                      
         GOTO1 VSUBR01,DMCB,('ELEMLSE',(R9)),(RC)                               
*                                                                               
SELMKT20 ZIC   R0,0(R3)            PT TO NEXT SELECT FIELD                      
         AR    R3,R0                                                            
         B     SELMKT2                                                          
*                                                                               
SELMKTX  B     EXITS1                                                           
*        DROP  R4                                                               
         EJECT                                                                  
*============ SPDSP - DISPLAY SPOT MKTS FOR ACTN DISPLAY =============*         
*                                                                               
*              ONLY DISPLAY MKTS THAT APPEAR IN MKT OVR LIST                    
*                                                                               
SPDSP    DS    0H                                                               
         LA    R5,SAVESTR                                                       
         USING SVTD,R5                                                          
         LA    R3,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         TWAXC RMKMK1H,(R3),PROT=Y                                              
         OI    RMKSRCH+6,X'20'     DISALLOW ANY INPUT IN SRC FLD                
         CLI   SVTELLN,0                                                        
         BE    SPDSPX                                                           
         ZIC   R1,SVTELLN          #MKTS IN LIST                                
         OC    SVTMKT,SVTMKT       LAST MKT READ                                
         BNZ   *+10                                                             
         MVC   SVTMKT,SVTMKTS      NOTHING PREV DISPLAYED                       
         LA    R8,SVTMKTS                                                       
SPDSP10  CLC   0(2,R8),SVTMKT                                                   
         BE    SPDSP15                                                          
         LA    R8,2(R8)                                                         
         BCT   R1,SPDSP10          DECR # MKTS LEFT IN LIST                     
         DC    H'0'                SHOULD HAVE FOUND SOMETHING                  
*                                                                               
SPDSP15  ST    R1,NMKTS            #MKTS IN LIST TO BE DISPLAYED                
*                                  (R8) - MKT IN SVTMKTS LIST                   
         LA    R3,RMKMKA1H          1ST OUTPUT FIELD FOR MKT #/NAME             
*                                                                               
SPDSP30  DS    0H                                                               
         LA    R1,RMKMKALH          END OF SCREEN REACHED?                      
         CR    R3,R1                                                            
         BH    EXITS1                                                           
         EDIT  (B2,0(R8)),(4,MYMKT)   CONVERT MKT# TO CHAR                      
         OC    MYMKT,=C'0000'                                                   
*                                                                               
         LA    R4,WORK                                                          
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SVTMED                                                   
*******  MVC   MKTKAGY,SVAPHAGY                                                 
         MVC   MKTKAGY,SVTAGY                                                   
         MVC   MKTKMKT,MYMKT                                                    
*                                                                               
         L     RE,ABUFF                                                         
         ST    RE,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'STATION',WORK,,                       
         L     R1,ABUFF                                                         
         CLC   WORK(8),0(R1)                                                    
         BNE   SPDSP40                                                          
         L     R1,ABUFF                                                         
         EDIT  (B2,0(R8)),(4,8(R3))   CONVERT MKT# TO CHAR                      
*        MVC   8(4,R3),MKTKMKT-MKTREC(R1)                                       
         MVC   8+5(L'RMKMKA1-5,R3),MKTNAME-MKTREC(R1)                           
         OI    6(R3),X'80'                                                      
         MVC   SVTMKT,0(R8)        LAST MKT DISPLAYED                           
         LA    R3,RMKMKA2H-RMKMKA1H(R3)   NEXT PROT FIELD                       
*                                                                               
SPDSP40  LA    R8,2(R8)             NEXT MKT IN SVTMKT LIST                     
         L     R1,NMKTS                                                         
         BCTR  R1,0                                                             
         ST    R1,NMKTS                                                         
         LTR   R1,R1                                                            
         BNZ   SPDSP30                                                          
         XC    SVTMKT(4),SVTMKT     SET TO BEGIN OF LIST NEXT TIME              
*                                                                               
SPDSPX   B     EXITS1                                                           
         DROP  R4                                                               
         EJECT                                                                  
*======== SPCHG - DISPLAY ALL MKT NUMBERS FOR MKT OVERIDE LIST =======*         
*                                                                               
*              MARK THOSE ALREADY ON FILE                                       
*                                                                               
SPCHG    DS    0H                                                               
         LA    R8,2048(RC)         SET UP ANOTHER BASE REGISTER                 
         LA    R8,2048(R8)          FOR WORKD                                   
         USING WORKD+4096,R8                                                    
         LA    R5,SAVESTR                                                       
         USING SVTD,R5                                                          
         OC    SVTMKT(4),SVTMKT                                                 
         BNZ   *+10                                                             
         MVC   SVTMKT,=C'0000'                                                  
         LA    R3,RMKMKLH+(RMKMKA1H-RMKMK1H)  CLEAR SCREEN                      
         TWAXC RMKMK1H,(R3),PROT=Y                                              
*                                                                               
         LA    R3,RMKMK1H          1ST POSITION ON SCREEN FOR MKT               
         LA    R4,MYWORK           BUILD KEY                                    
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SVTMED                                                   
******   MVC   MKTKAGY,SVAPHAGY                                                 
         MVC   MKTKAGY,SVTAGY                                                   
         MVC   MKTKMKT,SVTMKT      LAST MKT READ ON PRIOR SCREEN                
         L     RE,ABUFF                                                         
         ST    RE,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'STATION',MYWORK,,                     
         B     SPCHG20                                                          
         DROP  R4                                                               
*                                                                               
SPCHG05  LA    R1,RMKMKLH          ADDR OF END OF SCREEN                        
         CR    R3,R1               HAVE WE REACHED END OF SCREEN?               
         BH    SPCHGX              YES- EXIT                                    
*                                                                               
SPCHG10  DS    0H                                                               
         L     R1,ABUFF                                                         
         ST    R1,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRSEQ',=C'STATION',MYWORK,,                     
*                                                                               
SPCHG20  L     R1,ABUFF                                                         
         CLC   MYWORK(2),0(R1)     SAME MEDIA & REC TYPE                        
         BNE   SPCHGX0             NO MORE MKTS TO DISPLAY                      
         CLC   MYWORK+6(2),6(R1)   SAME AGY?                                    
         BNE   SPCHG10             NO, READ SEQ UNTIL AGY MATCHES               
*                                                                               
         L     R4,ABUFF            DISPLAY MKT ON SCREEN                        
         USING MKTREC,R4                                                        
         MVC   SVTMKT,MKTKMKT                                                   
         PACK  DUB,MKTKMKT                                                      
         CVB   R1,DUB              CONVERT MKT TO BINARY                        
         STH   R1,FNDMKT           SET PARMS (FNDMKT AND REPL) TO               
         MVC   REPL,FNDMKT         SEARCH IF THIS MKT IN ON OUR RECD            
*         BAS   RE,ELEMLST          IF ALREADY ON FILE, MARK WILL ='X'          
         GOTO1 VSUBR01,DMCB,('ELEMLSE',(R9)),(RC)                               
         CLI   MARK,C'X'                                                        
         BNE   *+12                                                             
         MVI   8(R3),C'X'                                                       
         OI    6(R3),X'80'                                                      
         LA    R3,RMKMKA1H-RMKMK1H(R3)                                          
*                                                                               
         EDIT  (B2,FNDMKT),(4,8(R3))   DISPLAY MKT# ON SCREEN                   
         MVC   8+5(L'RMKMKA1-5,R3),MKTNAME                                      
         OI    6(R3),X'80'                                                      
         LA    R3,RMKMK2H-RMKMKA1H(R3)  BUMP TO NEXT UNRPROT FIELD              
         B     SPCHG05             LOOP TO DO ALL MKTS TILL SCRN FULL           
*                                                                               
SPCHGX0  MVC   SVTMKT,=C'0000'     NO MORE MKTS - RESET TO TOP                  
*                                                                               
SPCHGX   B     EXITS1                                                           
         DROP  R4,R5,R8                                                         
         EJECT                                                                  
*=========== CALLSCR - SET LIST SCREEN FOR MKT/SPT DISPLAY ===========*         
*                                                                               
CALLSCR  DS    0H                                                               
         MVI   TWANUM,2            SAVE CURRENT SCRN IN TWA2                    
         MVC   COMAND2,=CL8'DMWRT   '                                           
*         BAS   RE,TWAIO            SAVE TWA                                    
         GOTO1 VSUBR01,DMCB,('TWAIE',(R9)),(RC)                                 
         GOTO1 CALLOV,DMCB,(X'A5',BASTABH),0,0   LOAD NEW SCREEN                
         B     EXITS1                                                           
         EJECT                                                                  
*=================== TWAIO- SAVE/RESTORE TWA FIELD ===================*         
*                                                                               
TWAIO    DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         ZIC   R3,TWANUM                                                        
         SLL   R3,32-8                                                          
         ICM   R3,3,TRMNUM                                                      
         LH    R4,=H'2400'                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'0960'        READ 2400 BYTES                       
         GOTO1 VDATAMR,DMCB,COMAND2,=C'TEMPSTR',(R3),ATWA                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
TWAIOX   B     EXITS1                                                           
         EJECT                                                                  
*================= LITERARY POOL & CONSTANTS FOR S1 ==================*         
         LTORG                                                                  
         SPACE 1                                                                
         SPACE 1                                                                
EXITS1   XIT1                                                                   
         DROP  R2,R9,RB,RC                                                      
***********************************************************************         
         TITLE 'CTLFM1A <==> TA021A - DEMO CONTROL RECORDS : SUBR02'            
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
SUBR02Q  EQU   ((((*-SUBR01)/4096)+1)*4096)                                     
         ORG   SUBR01+SUBR02Q                                                   
SUBR02   CSECT                                                                  
         NMOD1 0,**SR02**,RR=RA                                                 
         L     R9,0(R1)                                                         
         USING LFMTEMP,R9          R9=A(GLOBAL TEMP W/S)                        
         L     RC,4(R1)                                                         
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         L     R2,ATWA                                                          
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         ST    RA,RELO2            RELOCATION FACTOR                            
         L     R1,0(R1)                                                         
         SRL   R1,24                                                            
         SLL   R1,2                                                             
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
         SPACE 1                                                                
SETBUE   EQU   (SETBU#-*)/4+1                                                   
DSPMKE   EQU   (DSPMK#-*)/4+1                                                   
GETALPHE EQU   (GETALPH#-*)/4+1                                                 
UNKMKTE  EQU   (UNKMKT#-*)/4+1                                                  
INFOLSE  EQU   (INFOLS#-*)/4+1                                                  
INFSEE   EQU   (INFSE#-*)/4+1                                                   
WKTODE   EQU   (WKTOD#-*)/4+1                                                   
         SPACE 1                                                                
SETBU#   B     SETBUF                                                           
DSPMK#   B     DSPMKT                                                           
GETALPH# B     GETALPHA                                                         
UNKMKT#  B     UNKMKTS                                                          
INFOLS#  B     INFOLST                                                          
INFSE#   B     INFSEL                                                           
WKTOD#   B     WKTODT                                                           
         EJECT                                                                  
*=====================================================================*         
* SETBUF - DEPENDING ON WHERE CURSOR IS, SET SVTLSTYP TO THE TYPE OF            
*          MARKET LIST REQUESTED IF PF2 HIT.                                    
*                                                                               
SETBUF   DS    0H                                                               
         LA    R3,SAVESTR                                                       
         USING SVTD,R3                                                          
         LA    RE,KEY                                                           
         USING CTRREC,RE                                                        
         MVC   SVTAGY,CTRKAGY                                                   
******   MVC   SVTAGY,SVAPHAGY                                                  
         MVC   SVTSRC,CTRKSRC                                                   
         MVC   SVTMED(1),CTRKMED                                                
         MVC   SVTBOOK,CTRKBOOK                                                 
         MVC   SVTBTYP,CTRKCODE                                                 
         MVC   SVTACT,TMPACT                                                    
         MVC   SVTKEY(19),KEY                                                   
         MVC   SVTRSTAT,RSTAT      RECORD STATUS BYTE                           
         DROP  RE                                                               
*                                                                               
         LA    RE,DEMOMARH                                                      
         ZICM  RF,2(RE),(3)                                                     
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         C     RF,CURPOS                                                        
         BH    SETBUF5                                                          
         MVI   SVTLSTYP,X'01'      MARKET OVERRIDE LIST                         
         OI    CNCT,X'01'          IF NOT CNCTD TO AGY,DISALLOW LSTING          
         B     SETBUFX                                                          
*                                                                               
SETBUF5  LA    RE,DEMIMARH                                                      
         ZICM  RF,2(RE),(3)                                                     
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         C     RF,CURPOS                                                        
         BH    *+12                                                             
         MVI   SVTLSTYP,CTRMTPI     INVALID MARKET LIST                         
         B     SETBUFX                                                          
*                                                                               
         MVI   SVTLSTYP,CTRMTPV     VALID MARKET LIST                           
*                                                                               
SETBUFX  MVC   SVTMTYP,SVTLSTYP    COPY LST TYPE TO MKT TYPE FLD                
         CLI   SVTMTYP,X'01'       MKT OVERIDE?                                 
         BNE   *+8                                                              
         MVI   SVTMTYP,C'?'                                                     
         B     EXITS2                                                           
         DROP  R3                                                               
         EJECT                                                                  
*=====================================================================*         
* DSPMKT - DISPLAY MARKETS IN ELEMENT IN APPROPRIATE FIELD ON SCREEN            
*        R5 - PTS TO MKT LIST IN SAVE STORAGE                                   
*                                                                               
DSPMKT   DS    0H                                                               
         ZIC   R7,0(R5)            #MKTS IN LIST                                
         LA    R4,SCANTBL          R4=A(SCAN TABLE)                             
         SR    R6,R6               R6=N'ENTRIES IN TABLE                        
         LA    R8,2(R5)            PT TO 1ST MKT IN LIST                        
         L     RF,=V(XSORT)        SORT THE LIST BEFORE DISPLAYING              
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R8),(R7),2,2,0                                        
         ZIC   R7,0(R5)                                                         
*                                                                               
         LA    RF,PROTV                                                         
         ST    RF,APROT                                                         
         LA    R1,DEMVMAXH                                                      
         ST    R1,FULL             FULL SET TO A(LAST) FLDH FOR FLD             
         LA    R1,DEMVMARH                                                      
         ST    R1,FADR             SAVE A(FIRST OUTPUT TWA FIELD)               
         CLI   1(R5),0             TYPE OF MARKETS IN LIST                      
         BNE   DSPMK3                                                           
         CLI   0(R5),80            MAX 80 MKTS IN FIELD                         
         BL    DSPMK10             FITS                                         
         B     DSPMK8              DOESN'T:PROTECT FLD & DISPLAY MSG            
*                                                                               
DSPMK3   LA    RF,PROTI                                                         
         ST    RF,APROT                                                         
         LA    R1,DEMIMAXH                                                      
         ST    R1,FULL             FULL SET TO A(LAST) FLDH FOR FLD             
         LA    R1,DEMIMARH                                                      
         ST    R1,FADR             SAVE A(FIRST OUTPUT TWA FIELD)               
         CLI   1(R5),X'FF'                                                      
         BNE   DSPMK4                                                           
         CLI   0(R5),80            MAX 80 MKTS IN FIELD                         
         BL    DSPMK10             FITS                                         
         B     DSPMK8              DOESN'T:PROTECT FLD & DISPLAY MSG            
*                                                                               
DSPMK4   MVI   0(R4),C' '          FOR MKT OVR, OUTPUT SRC 1ST                  
         MVC   1(7,R4),0(R4)                                                    
         L     RE,=A(SRCTAB)      TRANSLATE SOURCE CODE IN TABLE                
         A     RE,RELO                                                          
*                                                                               
DSPMK5   CLI   0(RE),0                                                          
         BNE   *+12                                                             
         MVI   0(R4),C'?'          UNDEFINED SOURCE                             
         B     DSPMK7                                                           
         CLC   L'SRCTAB-1(1,RE),1(R5)    SOURCE                                 
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     DSPMK5                                                           
         MVC   0(8,R4),0(RE)       FIRST OUTPUT ENTRY IS SOURCE                 
*                                                                               
DSPMK7   LA    R4,8(R4)                                                         
         LA    R6,1(R6)                                                         
         LA    RF,PROTO            MKT OVERIDE                                  
         ST    RF,APROT                                                         
         LA    R1,DEMOMAXH                                                      
         ST    R1,FULL             FULL SET TO A(LAST) FLDH FOR FLD             
         LA    R1,DEMOMARH                                                      
         ST    R1,FADR             SAVE A(FIRST OUTPUT TWA FIELD)               
         CLI   0(R5),140           MAX 140 MKTS CAN FIT ON MKT OVR LIST         
         BL    DSPMK10             PROTECT THE FIELD AND DISPLAY MSG            
*                                                                               
DSPMK8   L     R1,FADR             1ST TWA FIELD                                
         L     R6,FULL             LAST TWA FIELD                               
         TWAXC (R1),(R6)           CLEAR ENTIRE FIELD                           
         L     R1,APROT                                                         
         MVI   0(R1),C'P'          FIELD IS PROTECTED                           
         L     R1,FADR             TWAXC CLOBBERS R1                            
         MVC   8(26,R1),=C'USE PF2 TO VIEW THESE MKTS'                          
DSPMK9   OI    6(R1),X'20'         TO PROTECTED FOR NEXT INPUT                  
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF               NEXT FLDHDR                                  
         C     R1,FULL                                                          
         BNH   DSPMK9              PROT ALL LINES FOR THIS MKTTYPE              
         B     DSPMKTX                                                          
*                                                                               
DSPMK10  L     R1,FADR             CLEAR FIELD                                  
         L     R6,FULL             FULL=A(LAST TWA FIELD FOR MKTS)              
         TWAXC (R1),(R6)                                                        
         L     R1,APROT                                                         
         MVI   0(R1),0             SET BIT TO UNPROTECTED                       
         LA    R8,2(R5)            PT TO 1ST MKT                                
*                                                                               
DSPMK13  MVI   0(R4),C' '          EDIT MARKET NUMBER INTO SCAN TABLE           
         MVC   1(7,R4),0(R4)                                                    
*                                                                               
DSPMK14  ZICM  R1,0(R8),(3)                                                     
         LTR   R1,R1                                                            
         BNZ   *+12                                                             
         LA    R8,2(R8)            BUMP TO NEXT MKT--DON'T CNT EMPTY            
         B     DSPMK14                                                          
*         BAS   RE,GETALPHA         GET ALPHA MKT #                             
         GOTO1 VSUBR02,DMCB,('GETALPHE',(R9)),(RC)                              
         LA    R4,8(R4)            NEXT SCANTBL SLOT                            
         LA    R6,1(R6)                                                         
         LA    R8,2(R8)                                                         
         BCT   R7,DSPMK13         DO FOR NUMBER OF MARKETS                      
*                                                                               
         L     RF,=V(SCINKEY)      MOVE DATA INTO FIELDS                        
         A     RF,RELO                                                          
         ZIC   R7,0(R5)                                                         
         XC    DMCB,DMCB                                                        
         L     R0,FADR                                                          
         ST    R0,DMCB                                                          
         MVI   DMCB,4                                                           
         CLI   1(R5),0                                                          
         BE    DSPMK20                                                          
         CLI   1(R5),X'FF'                                                      
         BE    DSPMK20                                                          
         LA    R7,1(R7)            #MKTS +1 FOR SOURCE CODE                     
         MVI   DMCB,7              MKT OVR                                      
DSPMK20  GOTO1 (RF),DMCB,,(8,SCANTBL),(R7)                                      
         L     R6,FULL             A(LAST TWA FOR THIS FIELD)                   
         OC    61+8(10,R6),61+8(R6)                                             
         BNZ   DSPMK8              SET PF2 MESSAGE                              
*                                                                               
DSPMKTX  B     EXITS2                                                           
         EJECT                                                                  
*================= GETALPHA - GET ALPHA CODE FOR MKT =================*         
*             0(R8) = MKT #  (2BYTE)                                            
*             0(R4) = DESTINATION FOR ALPHA MKT                                 
*             0(R5) = BEGINING OF ELEMENT                                       
*                                                                               
GETALPHA DS    0H                                                               
         CLI   1(R5),CTRMTPI       ALPHA MKT FOR VAL/INV LISTS ONLY             
         BE    *+12                   NO ALPHA MKTS FOR MKT OVR LIST            
         CLI   1(R5),CTRMTPV                                                    
         BNE   GETALP50            MKT OVR - JUST EDIT OUTPUT MKT #             
*                                                                               
         L     RE,AREC                                                          
         CLI   CTRKMED-CTRREC(RE),C'N'   FOR NETWORK,GET ALPHA NETTBL           
         BNE   GETALP20                                                         
         L     R1,=A(NETTBL)           NETWORK ALPHA CODE                       
         A     R1,RELO                                                          
GETALP5  CLI   0(R1),X'FF'                                                      
         BE    GETALP50            NOT DEFINED, JUST OUTPUT MKT #               
         CLC   0(2,R8),0(R1)       MATCH ON MKT #?                              
         BE    *+12                                                             
         LA    R1,L'NETTBL(R1)                                                  
         B     GETALP5                                                          
         MVC   0(6,R4),2(R1)       MOVE IN ALPHA MKT NAME                       
         B     GETALPX                                                          
*                                                                               
GETALP20 DS    0H                  FILTER THRU CTFILE FOR ALPHA MKT             
         L     R6,ABUFF2           READ ALPHA RECORD INTO BUFF2                 
         USING CTDMREC,R6                                                       
         XC    0(25,R6),0(R6)      CLEAR KEY FIELD AREA                         
         L     RE,AREC                                                          
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,CTRKMED-CTRREC(RE)                                      
         MVC   CTDMKSRC,CTRKSRC-CTRREC(RE)                                      
         MVC   CTDMKBKT,CTRKCODE-CTRREC(RE)                                     
         MVC   CTDMKNUM,0(R8)        MOVE IN MARKET NUMBER                      
         MVC   BUFFKEY2,0(R6)                                                   
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'CTFILE ',BUFFKEY2,(R6)                
         B     GETALP30                                                         
*                                                                               
*ETALP25 GOTO1 VDATAMR,DMCB,=C'DMRSEQ',=C'CTFILE ',(R6),(R6)                    
*        B     GETALP30                                                         
*                                                                               
*&&DO                                                                           
GETALP30 CLC   BUFFKEY2(19),0(R6)                                               
         BNE   GETALP50            NO MATCH                                     
         LA    R1,BUFFKEY2                                                      
         CLC   CTDMKBKT,CTDMKBKT-CTDMREC(R1)  BOOK TYPE MATCH?                  
         BNE   GETALP25            NO, READ SEQ TILL FOUND                      
         CLC   CTDMKNUM,0(R8)       MKT NUMBER FOUND?                           
         BNE   GETALP25                                                         
*&&                                                                             
GETALP30 CLC   BUFFKEY2(L'CTDMKEY),0(R6)  SINCE WE SUPPLIED ENTIRE              
         BNE   GETALP50            KEY, LETS MATCH ON ENTIRE KEY                
         MVC   0(3,R4),CTDMKMKT    FOUND MARKET ALPHA                           
         B     GETALPX                                                          
*                                                                               
GETALP50 EDIT  (2,0(R8)),(4,0(R4)),ALIGN=LEFT                                   
*                                                                               
GETALPX  B     EXITS2                                                           
         DROP  R6                                                               
         EJECT                                                                  
*=============== UNKMKTS - UNKNOWN MARKETS IN ELEMENT ===============*          
*                                                                               
UNKMKTS  DS    0H                                                               
         LA    R4,SAVESTR                                                       
         USING SVTD,R4                                                          
         OC    SVTELLN,SVTELLN                                                  
         BZ    UNMKTX                                                           
         ZIC   R0,SVTELLN          NUMBER OF MKTS IN LIST                       
         LA    R4,SVTMKTS          PT TO MKT LIST                               
         DROP  R4                                                               
*                                                                               
UNKM5    L     R6,ABUFF            BUFFER CONTAINING GOOD  MKTS                 
UNKM7    CLC   0(2,R4),0(R6)       IS MKT IN BUFF?                              
         BE    UNKM15              KNOWN MARKET                                 
         LA    R6,26(R6)           BUMP BUFF ENTRY                              
         OC    0(2,R6),0(R6)                                                    
         BNZ   UNKM7               ANY MORE BUFF ENTRIES?                       
*                                                                               
UNKM10   L     R1,NMKTS            NUMBER OF MKTS IN BUFFER                     
         MH    R1,=H'26'           EMPTY BUCKET IN BUFF                         
         L     RF,ABUFF                                                         
         AR    RF,R1                                                            
         MVC   0(2,RF),0(R4)       MOVE IN UNKNOWN MKT #                        
         MVI   2(RF),X'EA'         FORCE UNKNOWNS AFTER 'Z'-MKT NAMES           
         MVC   3(15,RF),=C'**UNKNOWN MKT: '                                     
         LR    R1,R0               PREVENT COUNTER FROM EDIT CLOBBERING         
         EDIT  (2,0(R4)),(4,18(RF))                                             
         LR    R0,R1               RESTORE COUNTER                              
         L     R1,NMKTS            BUMP NMKTS COUNT                             
         LA    R1,1(R1)                                                         
         ST    R1,NMKTS                                                         
*                                                                               
UNKM15   LA    R4,2(R4)            NEXT MKT IN ELEMENT                          
         BCT   R0,UNKM5            SEARCH FROM TOP OF BUFF                      
*                                                                               
UNMKTX   B     EXITS2                                                           
         EJECT                                                                  
*====================================================================*          
*INFOLST-LIST THE KEYS OF ALL DCON RECDS THAT HAVE SAME AGY/SRC/MEDIA           
*        ACTIVATED BY PF1.  RETURN TO MAIN SCREEN WITH PF12                     
*                                                                               
INFOLST  DS    0H                                                               
         LA    R5,SAVESTR                                                       
         USING SVTD,R5                                                          
*                                                                               
         CLI   PFKEY,1             1ST TIME/ SCREEN LOADED?                     
         BE    INFLS5                                                           
*         BAS   RE,INFSEL           SELECT KEY FROM INFO LINE                   
         GOTO1 VSUBR02,DMCB,('INFSEE',(R9)),(RC)                                
         CLI   DMCB,X'01'          WAS SOMETHING SELECTED?                      
         BE    INFLSTX             YES, EXIT                                    
*                                                                               
INFLS5   LA    R3,RMKMKLH+(RMKMKA1H-RMKMK1H)    CLEAR LIST SCREEN               
         TWAXC RMKMK1H,(R3),PROT=Y                                              
         XC    BASHDR,BASHDR                                                    
         MVI   FERN,X'FE'                                                       
         MVC   BASHDR(21),=C'INFORMATION DISPLAYED'                             
         MVC   RMKMSGL,=CL30'ST.BOOK   BOOKTYPE  CLT. CODE'                     
         MVC   RMKMSGR,=CL30'ST.BOOK   BOOKTYPE  CLT. CODE'                     
         OI    BASHDRH+6,X'80'                                                  
         OI    RMKMSGRH+6,X'88'                                                 
         OI    RMKMSGLH+6,X'88'                                                 
         LA    R3,RMKMKA1H         1ST OUTPUT FIELD FOR MKT #/NAME              
         L     RF,ABUFF                                                         
         ST    RF,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'CTFILE',SVTKEY,,                      
         L     R1,ABUFF                                                         
         CLC   SVTKEY(19),0(R1)    NO RECORDS FOUND                             
         BNE   INFLS20                                                          
*                                                                               
INFLS10  LA    R1,RMKMKALH          END OF SCREEN REACHED?                      
         CR    R3,R1                                                            
         BH    INFLS25             SCREEN FULL                                  
*                                                                               
         L     R4,ABUFF                                                         
         USING CTRREC,R4                                                        
         OI    6(R3),X'80'         TRANSMIT THIS PROT FIELD                     
         MVC   WORK(2),CTRKSRC                                                  
         MVC   MYHALF,CTRKBOOK                                                  
         XC    MYHALF,=X'FFFF'                                                  
         XC    8(L'RMKMSGL,R3),8(R3)                                            
         USING LSTLND,R3                                                        
         GOTO1 VSUBR02,DMCB,('WKTODE',(R9)),(RC)                                
         ZIC   R1,DUB2             DUB2(1) = L(OUTPUT DATE)                     
         BCTR  R1,0                                                             
         EXMVC R1,LSTLBOOK,DUB     DUB = OUTPUT DATE                            
         SPACE 1                                                                
         CLI   CTRKCODE,X'FF'                                                   
         BE    *+10                                                             
         MVC   LSTLBKTP,CTRKCODE        BOOKTYPE                                
         CLC   CTRKCLI,=X'FFFFFF'                                               
         BE    *+10                                                             
         MVC   LSTLCLI,CTRKCLI          CLIENT                                  
         MVC   SVTKEY,CTRKEY            SAVE KEY OF LAST RECD DISPLAYED         
         LA    R3,RMKMKA2H-RMKMKA1H(R3) NEXT PROT FIELD                         
         DROP  R3                                                               
*                                                                               
         L     R1,ABUFF                                                         
         ST    R1,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRSEQ',=C'CTFILE',SVTKEY,,                      
         L     R1,ABUFF                                                         
         CLC   SVTKEY(19),0(R1)                                                 
         BNE   INFLS20             RESET SAVED KEY                              
         MVC   SVTKEY,0(R1)                                                     
         B     INFLS10             PRINT OUT RECD KEY INFO                      
*                                                                               
INFLS20  XC    SVTKEY,SVTKEY                                                    
         LA    R4,SVTKEY                                                        
         MVI   CTRKTYP,CTRKTEQU                                                 
         MVC   CTRKSRC,SVTSRC                                                   
         MVC   CTRKMED,SVTMED                                                   
         MVC   CTRKAGY,SVTAGY                                                   
*                                                                               
INFLS25  MVI   DMCB,0                                                           
*                                                                               
INFLSTX  B     EXITS2                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
*===================== INFSEL- GET KEY FOR LIST ======================*         
*                                                                               
INFSEL   DS    0H                                                               
         LA    RF,RMKMKLH          LAST KEY ON SCREEN                           
         LA    R3,RMKMK1H          1ST KEY ON SCREEN                            
         MVI   DMCB,0                                                           
*                                                                               
INFSEL2  CR    R3,RF               HAVE WE REACHED BOTTOM OF SCREEN?            
         BH    INFSELX             YES                                          
         CLI   5(R3),0             WAS THIS KEY SELECTED?                       
         BNE   INFSEL5             YES                                          
         ZIC   R0,0(R3)            NEXT FIELD (KEY FLD)                         
         AR    R3,R0                                                            
         B     INFSEL7                                                          
*                                                                               
INFSEL5  CLI   8(R3),C' '          IGNORE SPACE CHARACTER                       
         BE    INFSEL7                                                          
         ZIC   R0,0(R3)            YES, PT TO NXT FLD- CONTAINS KEY             
         AR    R3,R0                                                            
         CLI   8(R3),0                                                          
         BNE   INFSEL8             IF PROT FLD IS NOT EMPTY, BUILD KEY          
*                                                                               
INFSEL7  ZIC   R0,0(R3)            PT TO NEXT SELECT FIELD                      
         AR    R3,R0                                                            
         B     INFSEL2                                                          
*                                  --A RECORD WAS SELECTED--                    
INFSEL8  XC    KEY,KEY             BUILD NEW KEY                                
         LA    R5,SAVESTR                                                       
         USING SVTD,R5                                                          
         LA    R4,KEY              BUILD KEY                                    
         USING CTRREC,R4                                                        
         USING LSTLND,R3                                                        
         MVC   KEY(19),SVTKEY                                                   
         SPACE 1                                                                
         LA    RE,LSTLBOOK         CONVERT START DATE                           
         LA    RF,LSTLBOOK+(L'LSTLBOOK-1)                                       
INFSEL8A CLI   0(RF),C'0'           GET ONLY THE BOOK PART ON THE LINE          
         BL    INFSEL8B              SEARCH BACKWARDS FOR 1ST NUMERIC           
         CLI   0(RF),C'9'                                                       
         BNH   INFSEL8C                                                         
INFSEL8B BCTR  RF,0                                                             
         CR    RE,RF               IF TRAVERSED BACK TO START,                  
         BL    INFSEL8A                                                         
         DC    H'0'                 THEN BAD ERROR!                             
INFSEL8C SR    RF,RE                                                            
         EXMVC RF,DUB,LSTLBOOK                                                  
*^^         MVC   FLD(6),8(R3)        CONVERT START DATE                        
         LA    RF,1(RF)                                                         
         STC   RF,WORK+2           SET L(DATE)                                  
         MVC   WORK(2),CTRKSRC     SET SOURCE/MEDIA                             
         GOTO1 VSUBR01,DMCB,('DTTOWE',(R9)),(RC)                                
         BZ    *+6                                                              
         DC    H'0'                SHOULD BE NO ERRORS                          
         XC    MYHALF,=X'FFFF'     MYHALF = RETURNED BOOK                       
         MVC   CTRKBOOK,MYHALF                                                  
*                                                                               
         MVI   CTRKCODE,X'FF'      BOOKTYPE                                     
         CLI   LSTLBKTP,0                                                       
         BE    *+10                                                             
         MVC   CTRKCODE,LSTLBKTP                                                
*                                                                               
         MVC   CTRKCLI(3),=X'FFFFFF'  CLIENT CODE                               
         CLI   LSTLCLI,0                                                        
         BE    *+10                                                             
         MVC   CTRKCLI(3),LSTLCLI                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,(X'E5',BASTABH),0,0     LOAD MAIN SCREEN             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BASKEYH                                                       
INFSEL9  ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   INFSEL9                                                          
         MVC   1(2,R1),=X'0101'                                                 
*                                                                               
         LA    R4,KEY                                                           
         OI    DEMSRCH+6,X'80'     DISPLAY KEY                                  
         L     RE,=A(SRCTAB)       GET SOURCE CODE                              
         A     RE,RELO                                                          
INFSEL10 CLI   0(RE),0                                                          
         BE    INFSEL15                                                         
         CLC   CTRKSRC,8(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     INFSEL10                                                         
         MVC   DEMSRC,0(RE)                                                     
         OI    DEMSRCH+6,X'80'                                                  
*                                  GET MEDIA CODE                               
INFSEL15 L     RE,=A(MEDTAB)                                                    
         A     RE,RELO                                                          
INFSEL17 CLI   0(RE),0             TEST E-O-T                                   
         BE    INFSEL20                                                         
         CLC   CTRKMED,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     INFSEL17                                                         
         MVC   DEMSUBF,1(RE)                                                    
         OI    DEMSUBFH+6,X'80'                                                 
*                                  MOVE IN AGENCY CODE                          
******   MVC   DEMAGY,CTRKAGY                                                   
         CLI   USIDFLAG,USIDBIN                                                 
         BE    INFSEL18                                                         
         MVC   DEMAGY(L'SVAPHAGY),SVAPHAGY                                      
         OI    DEMAGYH+6,X'80'                                                  
         B     INFSEL20                                                         
INFSEL18 DS    0H                MOVE IN BINARY CODE AS HEX IN SCREEN           
*                                                                               
         MVC   DEMAGY(L'SVSCRNID),SVSCRNID                                      
         OI    DEMAGYH+6,X'80'                                                  
*                                                                               
*                                                                               
INFSEL20 OI    DEMCODEH+6,X'80'    BOOKTYPE                                     
         CLI   CTRKCODE,X'FF'                                                   
         BE    *+10                                                             
         MVC   DEMCODE,CTRKCODE                                                 
*                                                                               
         OI    DEMCLIH+6,X'80'                                                  
         CLC   CTRKCLI,=X'FFFFFF'  CLIENT CODE                                  
         BE    *+10                                                             
         MVC   DEMCLI(3),CTRKCLI                                                
*                                                                               
         MVC   WORK(2),CTRKSRC     GET SOURCE/MEDIA                             
         MVC   MYHALF,CTRKBOOK                                                  
         XC    MYHALF,=X'FFFF'                                                  
         GOTO1 VSUBR02,DMCB,('WKTODE',(R9)),(RC)                                
         XC    DEMBOOK,DEMBOOK                                                  
         ZIC   R1,DUB2             DUB2(1) = L(OUTPUT DATE)                     
         BCTR  R1,0                                                             
         EXMVC R1,DEMBOOK,DUB      DUB = OUTPUT DATE                            
         OI    DEMBOOKH+6,X'80'                                                 
*                                                                               
         GOTO1 AREAD               READ THE KEY                                 
         MVI   DMCB,X'01'          SET DMCB TO CALL DISP ON EXIT                
*                                                                               
INFSELX  B     EXITS2                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
*====================== WEEK TO DATE CONVERSION ======================*         
*                                                                               
*     AT ENTRY,                                                                 
*        WORK(2) = SOURCE/MEDIA                                                 
*        MYHALF = YEAR/WEEK                                                     
*     AT EXIT,                                                                  
*        DUB    = DATE IN MMM/YY OR MMMDD/YY FORMAT                             
*                                                                               
WKTODT   DS    0H                                                               
         L     RE,=A(SMTAB)                                                     
         A     RE,RELO2                                                         
*                                                                               
WTD10    CLI   0(RE),0             IF END-OF-TABLE,                             
         BNE   *+6                                                              
         DC    H'0'                 DIE!                                        
         CLC   0(2,RE),WORK                                                     
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     WTD10                                                            
         ICM   R1,15,(SMAWD-SMTAB)(RE)                                          
         BZ    *+8                                                              
         A     R1,RELO2                                                         
         ST    R1,AWKDT            A(WEEK TO DATE CONVRSN)                      
         MVC   WORK+2(L'SMPNUM),(SMPNUM-SMTAB)(RE)   PARAM #                    
         SPACE 1                                                                
         XC    MYDATE,MYDATE                                                    
         OC    AWKDT,AWKDT         NEED SPECIAL CONVERSION?                     
         BNZ   WTD20                YES                                         
*                                                                               
* MMM/YY BOOK                                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,MYHALF),(0,MYDATE)                               
*                                                                               
*        ZIC   R1,MYHALF                                                        
*        CVD   R1,DUB2                                                          
*        UNPK  MYDATE(2),DUB2      YEAR                                         
*        ZIC   R1,MYHALF+1                                                      
*        CVD   R1,DUB2                                                          
*        UNPK  MYDATE+2(2),DUB2    MONTH                                        
*                                                                               
         OC    MYDATE,=C'000000'                                                
         MVI   DUB2,9                                                           
         B     WKTODTX             CONVERT TO MMM/YY AND EXIT                   
*                                                                               
* MMMDD/YY BOOK                                                                 
*                                                                               
WTD20    LA    R4,MYHALF                                                        
         CLI   WORK+2,2            SEE WHICH DATE TO WEEK CONV TO USE           
         BE    WTD20A                                                           
         SPACE 1                                                                
         MVI   DUB2,C'D'            USE NSIWEEK                                 
         L     R5,GETDAY                                                        
         L     R6,ADDAY                                                         
         L     R7,VDATCON                                                       
         B     WTD25                                                            
*                                                                               
WTD20A   MVI   DUB2,0               USE NETUNWK                                 
         LA    R5,MYDATE                                                        
         L     R6,GETDAY                                                        
         L     R7,ADDAY                                                         
*                                                                               
WTD25    GOTO1 AWKDT,DMCB,(DUB2,(R4)),(R5),(R6),(R7)                            
         ZIC   RE,WORK+2           R0=PARM # OF A(RETURNED DATE),               
         BCTR  RE,0                 LESS ONE,                                   
         SLL   RE,2                 TIMES 4                                     
         AR    R1,RE                                                            
         L     R1,0(R1)            R1-->DATE                                    
         MVC   MYDATE,0(R1)        MYDATE = RETURNED DATE (YYMMDD)              
         MVI   DUB2,8                                                           
*                                                                               
WKTODTX  GOTO1 VDATCON,DMCB,(X'80',MYDATE),(DUB2,DUB),0                         
         MVC   DUB2(1),4(R1)       L(OUTPUT DATE) IN DUB2                       
         B     EXITS2                                                           
         EJECT                                                                  
*================= LITERARY POOL & CONSTANTS FOR S2 ==================*         
         LTORG                                                                  
         SPACE 1                                                                
         SPACE 1                                                                
EXITS2   XIT1                                                                   
         DROP  R2,R9,RB,RC                                                      
***********************************************************************         
         TITLE 'CTLFM1A <==> TA021A - DEMO CONTROL RECORDS : SUBR03'            
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
SUBR03Q  EQU   ((((*-SUBR02)/4096)+1)*4096)                                     
         ORG   SUBR02+SUBR03Q                                                   
SUBR03   CSECT                                                                  
         NMOD1 0,**SR03**,RR=RA                                                 
         L     R9,0(R1)                                                         
         USING LFMTEMP,R9          R9=A(GLOBAL TEMP W/S)                        
         L     RC,4(R1)                                                         
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         L     R2,ATWA                                                          
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         ST    RA,RELO3            RELOCATION FACTOR                            
         L     R1,0(R1)                                                         
         SRL   R1,24                                                            
         SLL   R1,2                                                             
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
         SPACE 1                                                                
VALMRKE  EQU   (VALMRK#-*)/4+1                                                  
VALNEE   EQU   (VALNE#-*)/4+1                                                   
VALSPE   EQU   (VALSP#-*)/4+1                                                   
VALDME   EQU   (VALDM#-*)/4+1                                                   
VALALPE  EQU   (VALALP#-*)/4+1                                                  
VALBUSR  EQU   (VALBUR#-*)/4+1                                                  
         SPACE 1                                                                
VALMRK#  B     VALMRKT                                                          
VALNE#   B     VALNET                                                           
VALSP#   B     VALSPT                                                           
VALDM#   B     VALDMD                                                           
VALALP#  B     VALALPH                                                          
VALBUR#  B     VALBUSER                                                         
         EJECT                                                                  
*==================== VALIDATE A LIST OF MARKETS =====================*         
*                                                                               
* PARMS:   R6      = A(FIRST TWA INPUT FIELD)                                   
*          R7      = N'INPUT FIELDS                                             
*          MKTTYPE = TYPE OF LIST TO BE VALIDATED                               
*          AMKT    = A(WHERE TO SAVE MKT LIST IN SAVED STRG)                    
*                                                                               
* ON EXIT FERN=ERROR NUMBER FOR INVALID FIELD (FADR & FNDX SET)                 
*          FERN=X'FF' IF OK (ELEMENT ADDED TO RECORD)                           
*NOTE:  PROTECTED FIELDS ARE NOT VALIDATED HERE.                                
*                                                                               
VALMRKT  DS    0H                                                               
         TM    1(R6),X'20'         IS FIELD PROTECTED?                          
         BO    VALM20              YES, EXIT                                    
*                                                                               
*PROCESS UNPROTECTED FIELDS ONLY - COPY DATA TO AREA SET BY AMKT                
*IF FIELD IS PROTECTED, DATA IS ALREADY IN STRG LIST (AMKT)                     
*                                                                               
         USING CTRREC,R4                                                        
VALM1    ST    R6,DUB                                                           
         XC    MRKTLST(4),MRKTLST                                               
         XC    TEMP,TEMP           SAVE MKTS IN TEMP                            
*??         MVI   TEMP,0                                                        
         MVI   TEMP,X'04'                                                       
         MVI   TEMP+1,3                                                         
         MVC   TEMP+2(1),MKTTYPE                                                
         MVI   FLAG,0              SET FIRST TIME FLAG                          
*                                  VALIDATE AN INPUT FIELD                      
VALM2    LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    VALM16                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(80,SCANTBL)                                  
         CLI   4(R1),0                                                          
         BE    EIIFF                                                            
         MVC   NLINES,4(R1)        SAVE NUMBER OF -INPUT FIELDS                 
         MVI   FNDX,1                                                           
         LA    R8,SCANTBL          R8=A(SCAN BLOCK ENTRY)                       
*                                                                               
VALM4    CLC   FNDX,NLINES                                                      
         BH    VALM16                                                           
         CLI   1(R8),0             L'RHS                                        
         BNE   EIIFF                                                            
         CLI   FLAG,0              TEST FIRST TIME FLAG                         
         BNE   VALM8                                                            
         MVI   FLAG,1                                                           
         CLI   MKTTYPE,1           TEST IF MARKET OVERRIDE LIST                 
         BNE   VALM8                                                            
*                                  YES - FIRST FIELD MUST BE SOURCE             
         ZIC   RF,0(R8)                                                         
         BCTR  RF,0                RF=L'INPUT FIELD-1                           
         L     RE,=A(SRCTAB)       RE=A(SOURCE TABLE)                           
         A     RE,RELO                                                          
VALM6    CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIFF                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R8)                                                   
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     VALM6                                                            
         MVC   TEMP+2(1),L'SRCTAB-1(RE)                                         
         CLC   TEMP+2(1),CTRKSRC   CAN'T BE SAME AS KEY SOURCE                  
         BE    EIIFF                                                            
         B     VALM14                                                           
*                                                                               
VALM8    DS    0H                  VALIDATE MARKET NUMBERS/ALPHA                
         XC    FNDMKT,FNDMKT                                                    
         TM    2(R8),X'80'         C'LHS (NUMERIC)                              
         BO    VALM8B                                                           
         CLI   MKTTYPE,1           NO ALPHA MKTS FOR MKT OVR LIST?              
         BE    EIIFF               NO                                           
         CLI   CTRKMED,C'N'        IF NETWORK, VAL AGAINST NETTBL               
         BNE   VALM8A                                                           
*         BAS   RE,VALNET           NETWORK ALPHA VALIDATION                    
         GOTO1 VSUBR03,DMCB,('VALNEE',(R9)),(RC)                                
         B     VALM8B                                                           
*         BAS   RE,VALALPH          VALIDATE ALPHA MARKET FOR NON-NET           
VALM8A   GOTO1 VSUBR03,DMCB,('VALALPE',(R9)),(RC)                               
*                                                                               
VALM8B   OC    4(4,R8),4(R8)       C'LHS (NUMERIC)                              
         BZ    EIIFF                                                            
         CLC   4(4,R8),=F'9999'                                                 
         BH    EIIFF                                                            
*                                                                               
VALM8C   MVC   FNDMKT,6(R8)        VALIDATE MKT # AGAINST FILES                 
         CLI   MKTTYPE,1           VAL MKT OVR LIST AGAINST SPT FILE            
         BNE   VALM8D              NO                                           
         TM    CNCT,X'80'          ARE WE CNCTD TO THE RIGHT AGY?               
         BO    VALM9A              NO, BYPASS VALIDATION                        
*         BAS   RE,VALSPT                                                       
         GOTO1 VSUBR03,DMCB,('VALSPE',(R9)),(RC)                                
         BZ    EIIFF                                                            
         B     VALM9A                                                           
*                                                                               
VALM8D   CLI   CTRKMED,C'N'        NETWORK GETS VAL AGAINST TABLE               
         BNE   VALM9A                                                           
*         BAS   RE,VALNET                                                       
         GOTO1 VSUBR03,DMCB,('VALNEE',(R9)),(RC)                                
*        B     *+8                                                              
*        BAS   RE,VALDMD           VAL INVALID/VALD MKTS AGAINST DEMAND         
*        BZ    EIIFF               IF CC=0, MKT NOT FOUND                       
*                                  ENSURE MARKET NOT PREVIOUSLY DEFINED         
VALM9A   LA    RE,MRKTLST                                                       
VALM10   OC    0(2,RE),0(RE)                                                    
         BZ    VALM12                                                           
         CLC   6(2,R8),0(RE)                                                    
         BE    EDIFF               DUPLICATE FIELD                              
         LA    RE,2(RE)                                                         
         B     VALM10                                                           
*                                                                               
VALM12   MVC   0(2,RE),6(R8)       ADD MARKET TO LIST                           
         XC    2(2,RE),2(RE)                                                    
         ZIC   RF,TEMP+1           ADD MARKET TO ELEMENT                        
         LA    RE,TEMP(RF)                                                      
         MVC   0(2,RE),6(R8)                                                    
         LA    RF,2(RF)                                                         
         STC   RF,TEMP+1                                                        
*                                  BUMP TO NEXT SCAN BLOCK ENTRY                
VALM14   ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R8,L'SCANTBL(R8)                                                 
         B     VALM4                                                            
*                                  BUMP TO NEXT INPUT FIELD                     
VALM16   ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         TM    1(R6),X'20'         IGNORE PROT FIELDS                           
         BO    VALM16                                                           
         BCT   R7,VALM2                                                         
*                                  ALL FIELDS PROCESSED                         
         CLI   TEMP+1,3            ANY INPUT                                    
         BH    VALM18                                                           
         CLC   TEMP+2(1),MKTTYPE                                                
         BE    VALM17                                                           
         L     R6,DUB                                                           
         MVI   FNDX,2                                                           
         B     EMIFF                                                            
*                                                                               
VALM17   L     RF,AMKT                                                          
         MVI   0(RF),0             INDICATE LIST IS EMPTY                       
         B     VALM20                                                           
*                                  SAVE MKTS IN LIST PTD TO BY AMKT             
VALM18   DS    0H                                                               
         ZIC   R1,TEMP+1           LENGTH OF "ELEMENT"                          
         SH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         L     RF,AMKT             SAVE MKT IN APPROP STRG AREA                 
         EXMVC R1,2(RF),TEMP+3                                                  
         MVC   1(1,RF),TEMP+2      SAVE TYPE CODE                               
         LA    R1,1(R1)            RESTORE -1 FROM EXMVC                        
         SRL   R1,1                R1=# MKTS                                    
         STC   R1,0(RF)            SAVE # MKTS IN LIST                          
*                                  SET VALID EXIT                               
VALM20   MVI   FERN,X'FF'          NO ERRORS                                    
         MVI   FNDX,0                                                           
         B     VALMKTX                                                          
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
EMIFF    MVI   FERN,1              MISSING INPUT FIELD                          
         B     VALMKTX                                                          
EIIFF    MVI   FERN,2              INVALID INPUT FIELD                          
         B     VALMKTX                                                          
EDIFF    MVI   FERN,18             DUPLICATE INPUT FIELD                        
         B     VALMKTX                                                          
*                                                                               
VALMKTX  B     EXITS3                                                           
         EJECT                                                                  
*====== VALNET - VALIDATE NETWORK MARKET NUMBER AGAINST NETTBL. ======*         
*             FNDMKT = MKT # TO VALIDATE                                        
*             R8     - PTS TO SCANTABLE ENTRY 12(R8)=ALPHA INPUT                
*                                                                               
VALNET   DS    0H                                                               
         L     R1,=A(NETTBL)                                                    
         A     R1,RELO                                                          
         CLI   0(R8),6             MAX #CHAR FOR MKT NAME                       
         BH    VALNET8             INVALID INPUT                                
         ZIC   RE,0(R8)            L'INPUT                                      
         BCTR  RE,0                                                             
VALNET5  CLI   0(R1),X'FF'                                                      
         BE    VALNET8                                                          
         CLC   FNDMKT,0(R1)        MATCH ON MKT #?                              
         BE    VALNETX                                                          
         EXCLC RE,12(R8),2(R1)      MATCH ON MKT NAME?                          
         BE    VALNET6                                                          
         LA    R1,L'NETTBL(R1)                                                  
         B     VALNET5                                                          
*                                                                               
VALNET6  MVC   FNDMKT,0(R1)        SAVE MKT NUMBER                              
         MVC   6(2,R8),FNDMKT                                                   
         B     VALNETX                                                          
*                                                                               
VALNET8  XC    FNDMKT,FNDMKT       MKT # NOT FOUND -- CC=ZERO                   
*                                                                               
VALNETX  OC    FNDMKT,FNDMKT       SET CONDITION CODE                           
         B     EXITS3                                                           
         EJECT                                                                  
*== VALSPT - DETERMINE IF MKT REQUESTED IS VALID- APPEARS ON SPOTFIL =*         
*              I.E. THERE IS A SPOTFIL MKT RECORD OUT THERE FOR IT.             
*              INPUT:     0(R8) - PTS TO SCAN BLOCK                             
*                                                                               
VALSPT   DS    0H                                                               
         LA    R4,KEY                                                           
         USING CTRREC,R4                                                        
         EDIT  (B2,6(R8)),(4,WORK)  CONVERT MKT # TO CHAR                       
         OC    WORK(4),=C'0000'                                                 
         MVC   MYMKT,WORK         CONVERT MKT TO CHAR                           
         XC    WORK(L'KEY),WORK                                                 
         LA    RE,WORK                                                          
         USING MKTREC,RE           BUILD MKT RECD KEY IN WORK                   
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,CTRKMED                                                  
         MVC   MKTKMKT,MYMKT                                                    
******   MVC   MKTKAGY,CTRKAGY                                                  
         MVC   MKTKAGY,SVAPHAGY                                                 
*                                                                               
         L     R1,ABUFF                                                         
         ST    R1,DMCB+12                                                       
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'STATION',WORK,,                       
         L     R1,ABUFF                                                         
         CLC   WORK(8),0(R1)                                                    
         BE    VALSPTX                                                          
         XC    MYMKT,MYMKT         CLR MKT IF NOT FOUND                         
*                                                                               
VALSPTX  OC    MYMKT,MYMKT         MKT IS VALID                                 
         B     EXITS3                                                           
         DROP  RE                                                               
         EJECT                                                                  
*====================================================================*          
* VALDMD  -    DETERMINE IF MKT REQUESTED IS VALID- APPEARS IN MKT              
*              LISTING RETURNED BY DEMAND                                       
*              INPUT:     FNDMKT     -     MKT # TO VALIDATE                    
*                                                                               
VALDMD   DS    0H                                                               
         L     R8,ABUFF            PT TO LIST OF MKTS READ                      
         L     R7,NMKTS            NUMBER MKTS IN LIST                          
         LTR   R7,R7                                                            
         BZ    VALDM10                                                          
*                                                                               
VALDM5   CLC   FNDMKT,0(R8)       MKT FOUND IN LIST?                            
         BE    VALDMDX             YES                                          
         LA    R8,26(R8)                                                        
         BCT   R7,VALDM5                                                        
*                                                                               
VALDM10  XC    FNDMKT,FNDMKT       CLEAR MKT # TO SIGNAL NOT FOUND              
*                                                                               
VALDMDX  OC    FNDMKT,FNDMKT       SET CC=0 IF INVALID                          
         B     EXITS3                                                           
         EJECT                                                                  
*====================================================================*          
* VALALPH -    VALIDATE INPUT ALPHA MKT AGAINST CTFILE GET THE MKT #            
*              R8 - PTS TO SCANTABLE ENTRY                                      
*                                                                               
VALALPH  DS    0H                                                               
         L     R6,ABUFF2           READ ALPHA RECORD INTO BUFF2                 
         USING CTDMREC,R6                                                       
         XC    0(25,R6),0(R6)      CLEAR KEY FIELD AREA                         
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,CTRKMED                                                 
         MVC   CTDMKSRC,CTRKSRC                                                 
         MVC   CTDMKBKT,CTRKCODE   BOOKTYPE                                     
         MVC   CTDMKMKT,12(R8)                                                  
         XC    4(4,R8),4(R8)       CLR NUMERIC PART OF SCAN ENTRY               
         MVC   BUFFKEY2,0(R6)                                                   
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'CTFILE ',BUFFKEY2,(R6)                
         CLC   BUFFKEY2(23),0(R6)                                               
         BNE   VALALPHX                                                         
         MVC   6(2,R8),CTDMKNUM    GET MARKET NUMBER                            
*                                                                               
VALALPHX B     EXITS3                                                           
         DROP  R6                                                               
         EJECT                                                                  
*====================================================================*          
* VALBUSER-  READS ID RECORD AND GETS AGENCY ALPHA CODE FROM ID NUMBER          
* INPUT: CTRKAGY SET TO BINARY USER ID                                          
* OUTPUT:SVAPHAGY SET TO TWO BYTE AGENCY CODE                                   
*  CC SET TO EQUAL IF VALID ID                                                  
*  CC SET TO NOT EQUAL IF INVALID ID                                            
*                                                                               
VALBUSER DS    0H                                                               
         L     R6,ABUFF2           READ ALPHA RECORD INTO BUFF2                 
         USING CTIREC,R6                                                        
         XC    0(25,R6),0(R6)      CLEAR KEY FIELD AREA                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTRKAGY                                                  
         MVC   BUFFKEY2,0(R6)                                                   
         GOTO1 VDATAMR,DMCB,=C'DMRDHI',=C'CTFILE ',BUFFKEY2,(R6)                
         CLC   BUFFKEY2(25),0(R6)                                               
         BNE   VALBNO                                                           
*                                                                               
         LA    RE,CTIDATA                                                       
VALBU20  CLI   0(RE),0                                                          
         BE    VALBNO                                                           
*                                                                               
         CLI   0(RE),CTAGYELQ       AGENCY ALPHA ID ELEMENT                     
         BE    VALBU40                                                          
         ZIC   R0,1(RE)             ELEMENT LENGTH                              
         AR    RE,R0                                                            
         B     VALBU20                                                          
VALBU40  DS    0H                                                               
         USING CTAGYD,RE                                                        
         MVC   SVAPHAGY,CTAGYID                                                 
*                                                                               
VALBYES  DS    0H                                                               
*                                                                               
         CR    RB,RB                                                            
         B     VALBUSRX                                                         
*                                                                               
VALBNO   DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         CR    RB,R0                                                            
*                                                                               
VALBUSRX B     EXITS3                                                           
         DROP  R6                                                               
         EJECT                                                                  
*================= LITERARY POOL & CONSTANTS FOR S3 ==================*         
         LTORG                                                                  
         SPACE 1                                                                
         SPACE 1                                                                
EXITS3   XIT1                                                                   
         DROP  R2,R4,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'CTLFM1A <==> TA021A - DEMO CONTROL RECORDS'                     
**********************************************************************          
* TABLES  -    FOR SOURCE, MEDIA,NET, ETC VALIDATION                            
**********************************************************************          
*                                  TABLE OF VALID SOURCES                       
SRCTAB   DS    0CL9                                                             
         DC    C'NSI     ',C'N'    NSI SPOT                                     
         DC    C'NTI     ',C'N'    NSI NETWORK                                  
         DC    C'NHT     ',C'H'    NSI HISPANIC                                 
         DC    C'ARB     ',C'A'    ARB SPOT                                     
         DC    C'MFX     ',C'M'    MEDIAFAX SPOT                                
         DC    C'BBM     ',C'A'    BBM SPOT                                     
         DC    C'BIRCH   ',C'N'    BIRCH SPOT                                   
         DC    C'SRC     ',C'S'    SRC SPOT                                     
         DC    C'BBRADIO ',C'M'    BBM RADIO                                    
         DC    C'RADAR   ',C'R'    RADAR NETWORK RADIO                          
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'T',CL8'TV'                                                     
         DC    C'T',CL8'USTV'                                                   
         DC    C'C',CL8'CANTV'                                                  
         DC    C'R',CL8'RADIO'                                                  
         DC    C'N',CL8'NETTV'                                                  
         DC    C'A',CL8'NAD'                                                    
         DC    C'B',CL8'CABLE'                                                  
         DC    C'W',CL8'WEEKLY'    WEEKLIES                                     
         DC    X'00'                                                            
*                                  TABLE OF VALID SOURCE/MEDIA COMBOS           
SMTAB    DS    0CL(L'SMSM+L'SMWM+L'SMADW+L'SMAWD+L'SMPNUM)                      
SMSM     DS    CL2                 SOURCE/MEDIA                                 
SMWM     DS    CL1                 WEEKLY OR MONTHLY                            
SMADW    DS    AL4                 A(DATE TO WEEK CNVRSN) OR NULLS              
SMAWD    DS    AL4                 A(WEEK TO DATE CNVRSN) OR NULLS              
SMPNUM   DS    AL1                 NTH PARM WHICH HAS A(RETURNED DATE)          
         ORG   SMTAB                                                            
         DC    C'AC',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'NC',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'AR',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'RR',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'MR',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'NR',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'AT',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'NT',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'MT',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'ST',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'NA',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'NN',C'W',VL4(NETWEEK),VL4(NETUNWK),AL1(2)                      
         DC    C'NB',C'W',VL4(NETWEEK),VL4(NETUNWK),AL1(2)                      
         DC    C'NW',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1)                      
         DC    C'HN',C'M',AL4(0),AL4(0),AL1(0)                                  
         DC    C'HW',C'W',VL4(NETWEEK),VL4(NETUNWK),AL1(2)                      
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
NETTBL   DS    0XL8                NETWORK MKTS                                 
         DS    0XL2,0CL6           2BYTE MKT #, 6BYTE ALPHA MKT NAME            
*NTI                                                                            
         DC    AL2(001),C'NTI   '  GRANTS ACCESS TO BOTH NTI + NTINSS           
         DC    AL2(002),C'NTIAGG'  NTI AGGR DATA (05 RECDS)                     
         DC    AL2(210),C'NTIC  '    N/A ANY LONGER                             
         DC    AL2(211),C'NTIA  '    N/A ANY LONGER                             
         DC    AL2(221),C'DAY   '  NTI DAILY DATA                               
         DC    AL2(222),C'DAYAGG'  NTI AGGR DAILY DATA (05 RECDS)               
         DC    AL2(215),C'WB1   '  WB TCAR                                      
         DC    AL2(216),C'TCAR  '  MAIN TCAR                                    
*                                                                               
*CABLE                                                                          
         DC    AL2(300),C'CABLE '  <--POST 9337                                 
         DC    AL2(010),C'CNN   '                                               
         DC    AL2(011),C'WTBS  '                                               
         DC    AL2(012),C'USA   '                                               
         DC    AL2(013),C'CBN   '                                               
         DC    AL2(014),C'ESPN  '                                               
         DC    AL2(015),C'MTV   '                                               
         DC    AL2(016),C'NICK  '                                               
         DC    AL2(017),C'AEN   '                                               
         DC    AL2(018),C'LFTM  '                                               
*NAD                                                                            
         DC    AL2(200),C'NAD   '  NAD MONTHLY                                  
         DC    AL2(201),C'NAW   '  NAD WEEKLY                                   
         DC    AL2(202),C'NADPC '  MONTHLY OPTIONAL PC DATA                     
         DC    AL2(203),C'NAWPC '  WKLY OPTIONAL PC DATA                        
         DC    AL2(205),C'FSTNAD'  FAST NAD WEEKLY DATA                         
         DC    AL2(207),C'MOVIE '  NAD WEEKLY MOVIEGOER                         
         DC    AL2(400),C'NADS  '  MONTHLY NAD-NSS                              
         DC    AL2(401),C'NAWS  '  WKLY NAD-NSS                                 
         DC    AL2(402),C'NADSPC'  MONTHLY NAD-NSS OPTIONAL PC DATA             
         DC    AL2(403),C'NAWSPC'  WKLY NAD-NSS OPTIONAL PC DATA                
*                                                                               
*NHTI                                                                           
         DC    AL2(500),C'NHTI  '                                               
         DC    AL2(501),C'NHTGMN'                                               
         DC    AL2(502),C'NHTAFF'                                               
*                                                                               
*NHT CABLE                                                                      
         DC    AL2(510),C'NHTGAL'  <--CABLE NHTI GALAVISION                     
         DC    AL2(511),C'NHTCBL'  <--CABLE NHTI                                
*                                                                               
         DC    X'FFFF',XL6'FF'                                                  
NETTBLQ  EQU   (*-NETTBL)/L'NETTBL   NUMBER OF NET TABLE ENTRIES                
         EJECT                                                                  
*                                  TABLE OF VALID PARAMETERS                    
PARMTAB  DS    0CL14                                                            
         DC    C'FAST    ',AL1(1,1,3),AL3(KEYFAST)                              
         DC    C'P+S2    ',AL1(2,1,3),AL3(KEYPPS2)                              
         DC    C'PROG    ',AL1(3,1,5),AL3(KEYPROG)                              
         DC    C'XQHR    ',AL1(4,1,4),AL3(KEYXQHR)                              
         DC    C'TPT     ',AL1(5,1,3),AL3(KEYTPT)                               
         DC    C'PAV     ',AL1(6,1,3),AL3(KEYPAV)                               
         DC    C'NTI     ',AL1(7,1,3),AL3(KEYNTI)                               
         DC    C'MPA     ',AL1(8,1,3),AL3(KEYMPA)                               
         DC    C'DPT     ',AL1(9,1,3),AL3(KEYDPT)                               
         DC    C'RDP     ',AL1(10,1,3),AL3(KEYRDP)                              
         DC    C'JUL211  ',AL1(11,1,5),AL3(KEY211)                              
         DC    C'SHARES  ',AL1(12,1,5),AL3(KEYSHR)                              
         DC    C'XSPILL  ',AL1(13,1,3),AL3(KEYXSPL)                             
         DC    C'PUTS    ',AL1(14,1,5),AL3(KEYPUTS)                             
         DC    C'ADI     ',AL1(15,1,3),AL3(KEYADIS)                             
         DC    C'S7MIN   ',AL1(16,1,3),AL3(KEYS7MIN)                            
         DC    C'E7MIN   ',AL1(17,1,3),AL3(KEYE7MIN)                            
         DC    C'SPDOM   ',AL1(18,1,3),AL3(KEYSPDOM)                            
         DC    X'00'                                                            
*                                  TABLE OF VALID KEYWORDS                      
KEYTAB   DS    0CL10                                                            
KEYFAST  DC    C'YES     ',AL1(0),X'40'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYPPS2  DC    C'YES     ',AL1(0),X'20'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYPROG  DC    C'2WEEK   ',AL1(0),X'10'                                         
         DC    C'4WEEK   ',AL1(0),X'08'                                         
         DC    C'DEFAULT ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYXQHR  DC    C'YES     ',AL1(0),X'04'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEY211   DC    C'YES     ',AL1(0),X'02'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYSHR   DC    C'TOTAL   ',AL1(0),X'01'                                         
         DC    C'QHR     ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYTPT   DC    C'YES     ',AL1(1),X'80'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYPAV   DC    C'YES     ',AL1(1),X'40'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYNTI   DC    C'YES     ',AL1(1),X'20'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYMPA   DC    C'YES     ',AL1(1),X'10'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYDPT   DC    C'YES     ',AL1(1),X'08'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYRDP   DC    C'YES     ',AL1(1),X'04'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYXSPL  DC    C'YES     ',AL1(2),X'80'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYPUTS  DC    C'2YEAR   ',AL1(2),X'40'                                         
         DC    C'1YEAR   ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYADIS  DC    C'YES     ',AL1(2),X'20'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYS7MIN DC    C'YES     ',AL1(2),X'10'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYE7MIN DC    C'YES     ',AL1(2),X'08'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYSPDOM DC    C'YES     ',AL1(2),X'04'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
* *********************************************************************         
*              DSECT TO COVER TEMP W/S                                          
* *********************************************************************         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
RELO1    DS    A                                                                
RELO2    DS    A                                                                
RELO3    DS    A                                                                
VSUBR01  DS    A                   A(FIRST SUBROUTINE POOL)                     
VSUBR02  DS    A                   A(SECOND SUBROUTINE POOL)                    
VSUBR03  DS    A                   A(THIRD SUBROUTINE POOL)                     
FLAG     DS    X                                                                
APROT    DS    A                   ADDRESS OF PROTECTED FIELD BIT               
ERRFLG   DS    X                   ERRORS DURING VALIDTN  0=NO ERRORS           
*                                    X'01'= DUPLICATE MKT                       
*                                    X'02' =INVALID MKT OVR SOURCE              
ERRMKT   DS    X                   MKT WHICH ERROR OCCURED FOR                  
MKTTYPE  DS    X                                                                
NLINES   DS    X                                                                
PSWOK    DS    X                   'N'- NOT PRESENT OR INVALID                  
PFKEY    DS    X                   FUNCTION KEY 1-12  ENTER=0                   
CURPOS   DS    F                   A(CURSOR) POSITION ON SCREEN                 
CNCT     DS    X                   X'80'= NOT CNCTD TO RIGHT AGY                
*                                  X'01'= MKT OVR LIST REQUESTED                
AMYIO    DS    A                   ADDRESS OF IOAREA                            
ABUFF    DS    A                   A(BUFF)                                      
ABUFF2   DS    A                   A(BUFF2)                                     
DEMAND   DS    A                   A(DEMAND)                                    
CALLOV   DS    A                   A(CALLOV)                                    
GETDAY   DS    A                   A(GETDAY)                                    
ADDAY    DS    A                   A(ADDAY)                                     
PERVAL   DS    A                   A(PERVAL)                                    
*                                                                               
MYMODE   DS    X                   VALIDATE OR DISPLAY                          
VALQ     EQU   X'01'                                                            
DISPQ    EQU   X'02'                                                            
*                                                                               
RSTAT    DS    X                   RECORD STATUS BYTE                           
TWANUM   DS    X                   FOR SAVING/RESTORING TWAS                    
TMPACT   DS    X                   ORIGINAL ACTION REQUESTED                    
DUPL     DS    C                   FLAG IF DUPLICATE MKT FOUND                  
MARK     DS    C                                                                
AMKT     DS    A                   ADDR WHERE TO SAVE MKT LIST                  
NMKTS    DS    F                   NUMBER OF MARKETS IN BUFFER                  
FNDMKT   DS    XL2                                                              
REPL     DS    XL2                                                              
MYMKT    DS    XL4                                                              
COMAND2  DS    CL8                                                              
OPTIONS  DS    CL20                                                             
FULL     DS    F                                                                
*VAPHAGY DS    CL2                 SAVE ALPHA AGENCY                            
*VBINAGY DS    CL2                 SAVE BINARY AGENCY  USER ID                  
*VSCRNID DS    CL(L'DEMAGY)        SAVE ID CHARS FROM SCREEN FIELD              
ADTWK    DS    A                   A(DATE TO WEEK CONVRSN)                      
AWKDT    DS    A                   A(WEEK TO DATE CONVRSN)                      
MYHALF   DS    H                                                                
MYDATE   DS    CL6                 6-BYTE EBCDIC DATE                           
*                                                                               
MRKTLST  DS    200CL2                                                           
SCANTBL  DS    90CL32                                                           
PERVOUT  DS    CL(PERVALDX-PERVALD)   PERVAL OUTPUT BLOCK                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 5                                                                
*                                                                               
DBXTUID  DS    0X                  DBLOCK EXTENSION FOR USER ID                 
         DS    CL4                                                              
         DS    A                                                                
         DS    H                                                                
*                                                                               
BUFFKEY2 DS    CL25                25 BYTE KEY FOR CTFILE                       
MYIO     DS    XL256                                                            
MYWORK   DS    XL64                                                             
*                                                                               
BUFF     DS    12000X                                                           
BUFFQ    EQU   *-BUFF              LENGTH OF BUFF                               
*                                                                               
BUFF2    DS    2000X                                                            
BUFF2Q   EQU   *-BUFF2             LENGTH OF BUFF2                              
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* *******************************************************************           
* SOME INCLUDES                                                                 
* *******************************************************************           
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* COMFACS                                                                       
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
* DBEXTRAD                                                                      
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* SPGENMKT                                                                      
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
         EJECT                                                                  
* ********************************************************************          
* CTLFME5D -  MAIN SCREEN                                                       
* ********************************************************************          
* CTLFME5D                                                                      
**       ++INCLUDE CTLFME5D                                                     
       ++INCLUDE CTLFME5AD                                                      
         ORG  CTLFMFFD+2400        SAVE AREA BETWEEN PF2 TRANSACTIONS           
SVAPHAGY DS    CL2                 SAVE ALPHA AGENCY                            
SVBINAGY DS    CL2                 SAVE BINARY AGENCY  USER ID                  
SVSCRNID DS    CL(L'DEMAGY)        SAVE ID CHARS FROM SCREEN FIELD              
USIDFLAG DS    X                   FLAG TEST IF USER ID ENTERED                 
USIDBIN  EQU   0                   BINARY USER ID ENTERED                       
USIDALP  EQU   1                   ALPHA ID ENTERED                             
SCRSTR   DS    0H                  BEGIN OF SCREEN STORAGE                      
NVMKT    DS    X                   #VALID MKTS                                  
VTYPE    DS    X                   MKT TYPE CODE = X'00'                        
VMKTS    DS    CL512               VALID MKTS                                   
LISTSZ   EQU   *-NVMKT             DISP TO NEXT MKT LIST                        
*                                                                               
NIMKT    DS    X                   #INVALID MKTS                                
ITYPE    DS    X                   MKT TYPE CODE = X'FF'                        
IMKTS    DS    CL512               INVALID MKTS                                 
*                                                                               
NOMKT    DS    X                   #OVERIDE MKTS                                
OTYPE    DS    X                   MKT TYPE CODE = 1CHAR SRC CODE               
OMKTS    DS    CL512               OVERIDE MKTS                                 
*                                                                               
PROTV    DS    X                   PROTECTED FIELD BIT                          
PROTI    DS    X                                                                
PROTO    DS    X                                                                
*                                                                               
LISTQ    EQU   *-SCRSTR                                                         
SAVESTR  DS    0H                                                               
*                                  USES SVTD DSECT                              
         EJECT                                                                  
* ********************************************************************          
* CTLFMA5 -  MARKET LIST SCREEN                                                 
* ********************************************************************          
         ORG   BASTABH                                                          
**       ++INCLUDE CTLFMA5D                                                     
       ++INCLUDE CTLFMA5D                                                       
         EJECT                                                                  
* ********************************************************************          
* SVTD - DSECT OF STORAGE AT BOTTOM OF SCREEN BETWEEN PF2 TRANSACTIONS          
* ********************************************************************          
SVTD     DSECT                     TWA SAVE FORMAT                              
SVTMKT   DS    CL21                LAST MARKET ON LAST SCREEN                   
SVTSRC   DS    CL1                 SOURCE                                       
SVTMED   DS    CL1                 MEDIA                                        
SVTAGY   DS    CL2                 AGENCY CODE                                  
SVTBTYP  DS    CL1                 BOOK TYPE (CTRKCODE)                         
SVTCLI   DS    CL3                 CLIENT                                       
SVTBOOK  DS    CL2                 BOOK                                         
SVTKEY   DS    CL25                LAST KEY READ DISPLAYED                      
SVTRSTAT DS    X                   RECORD STATUS BYTE                           
SVTACT   DS    X                   ACTION REQUESTED                             
SVTLSTYP DS    X                   TYPE OF LIST REQUESTED:0,1,X'FF'             
SVTEL04  DS    X                   BUILD MKT LIST ELMNT HERE                    
SVTELLN  DS    X                   LENGTH OF ELMNT                              
SVTMTYP  DS    X                   TYPE OF MARKET OR MKT OVR SOURCE             
SVTMKTS  DS    252CL2              UP TO 252 MARKETS (2BYTE MKT CODE)           
SVTMKTSX DS    C                   TERMINATOR FOR MARKET LIST                   
SVTRECQ  EQU   *-SVTD              DISPLACEMENT TO SAVED RECORD                 
*                                                                               
SVTREC   DS    XL1000              NEW RECORD MAX LENGTH ON CTFILE              
SVTDQ    EQU   *-SVTD              LENGTH OF TOTAL SAVED STRG USED              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*=========================== PERVAL DSECT ============================*         
         SPACE 1                                                                
       ++INCLUDE DDPERVALD                                                      
         SPACE 1                                                                
PERVALDX EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= MISCELLANEOUS DSECTS ========================*         
         SPACE 1                                                                
*-------------------------- INFO LIST-LINE ---------------------------*         
*                                                                               
LSTLND   DSECT                                                                  
         DS    CL8                 TWA FIELD HEADER                             
LSTLBOOK DS    CL8                 START BOOK                                   
         DS    CL6                                                              
LSTLBKTP DS    CL1                 BOOKTYPE                                     
         DS    CL7                                                              
LSTLCLI  DS    CL3                 CLIENT                                       
         DS    CL5                                                              
LSTLNQ   EQU   *-LSTLND                                                         
         DS    0CL((LSTLNQ-8)-L'RMKMKA1+1)                                      
         DS    0CL(L'RMKMKA1-(LSTLNQ-8)+1)                                      
         SPACE 1                                                                
*----------------------- X'04' MARKET CONTROL ------------------------*         
*                                                                               
CTRMD    DSECT                                                                  
CTRMECDE DS    CL1                 ELEMENT CODE                                 
CTRMECDQ EQU   X'04'                                                            
CTRMLEN  DS    XL1                 ELEMENT LENGTH                               
CTRMTYPE DS    XL1                 MARKET TYPE                                  
CTRMTPV  EQU   X'00'                VALID                                       
CTRMTPI  EQU   X'FF'                INVALID                                     
*TRMTPO  EQU   X(FROM SRCTAB)       OVERRIDE                                    
CTRMFXLQ EQU   *-CTRMD             FIXED OVERHEAD LENGTH                        
CTRMMKTS DS    0C                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145CTLFM1A   05/01/02'                                      
         END                                                                    
