*          DATA SET ACLFM18    AT LEVEL 052 AS OF 05/01/02                      
*PHASE T60318A,*                                                                
         TITLE 'T60318 - ACCOUNTING LIST RECORDS'                               
T60318   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ACCLSTX-ACCLSTD,*LFM18**,CLEAR=YES                               
         LR    R6,RC                                                            
         USING ACCLSTD,R6          PROGRAM WORKING STORAGE                      
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)            LFM WORKING STORAGE                          
         USING LOGWORKD,RC                                                      
LSTINIT  LA    RF,RELOC            OBTAIN RELOCATION FACTOR.                    
         S     RF,RELOC                                                         
         ST    RF,WRELOC           SAVE FACTOR.                                 
*                                                                               
         LA    RF,VTYPES           RELOCATE VTYPES, ETC.                        
         LA    RE,WVTYPES                                                       
LSTINT02 CLI   0(RF),X'FF'         END OF VTYPES.                               
         BE    LSTINT04            YEP.                                         
         L     R1,0(RF)            R1 CONTAINS VTYPE.                           
         A     R1,WRELOC           R1 CONTAINS RELOCATED VTYPE.                 
         ST    R1,0(RE)            SAVE IT.                                     
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         B     LSTINT02            GET THE NEXT ONE.                            
*                                                                               
         USING COMFACSD,RE                                                      
LSTINT04 L     RE,COMFACS                                                       
         MVC   GETFACT,CGETFACT                                                 
         MVC   GETDAY,CGETDAY                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   SCANNER,CSCANNER                                                 
         DROP  RE                                                               
LSTINTX  DS    0H                                                               
*                                                                               
*                                                                               
         CLI   MODE,BUILDKEY                                                    
         BE    LISTKEY                                                          
         CLI   MODE,DSPLYREC                                                    
         BE    LSTDSPLY                                                         
         CLI   MODE,BUILDREC                                                    
         BE    LSTBUILD                                                         
         DC    H'0'                                                             
OKXIT    MVI   ERROR,X'FF'                                                      
EXIT     XMOD1 1                                                                
*                                                                               
EXIT2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         USING ACKEYD,R8                                                        
LISTKEY  LA    R8,KEY                                                           
         MVC   ACLKEY,SPACES                                                    
         MVI   ACLKCODE,ACLKCEQU                                                
         MVC   ACLKCOMP,COMPANY                                                 
         LA    R2,LOGLCODH                                                      
         GOTO1 ANY                                                              
         TM    LOGLCODH+4,X'20'                                                 
         BO    *+12                NOT 1ST TIME, SKIP DISPLAY.                  
         OI    LOGLCODH+4,X'20'    MARK AS VALIDATED ON FIRST TIME.             
         MVI   ANYKEY,C'Y'         FORCE DISPLAY                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EXIT                                                             
         MVC   ACLKLIST(0),LOGLCOD                                              
         SPACE 2                                                                
*              DISPLAY THE LIST RECORD DETAILS.                                 
*                                                                               
         USING ACLISTD,R8                                                       
LSTDSPLY TWAXC LOGLYN1H,LOGTAB2H                                                
         LA    R8,IO2                                                           
         AH    R8,DATADISP         R8 = A(1ST ELEMENT)                          
         LA    R2,LOGLNM1H         R2 = A(SCREEN FIELD PRIOR TO NAME).          
         GOTO1 NAMOUT              DISPLAY THE NAME.                            
         XC    LCONTROL,LCONTROL   INITIALIZE FIRST TIME SWITCH ETC.            
         MVI   LFSTEL,1                                                         
LSTD020  CLI   ACLIEL,0                                                         
         BE    LSTDEND                                                          
         CLI   ACLIEL,ACLIELEQ                                                  
         BE    LSTD040             LIST TYPE ELEMENT.                           
         CLI   ACLIEL,ACLDELEQ                                                  
         BE    LSTD060             LIST DATA ELEMENT.                           
LSTD030  ZIC   R1,ACLILEN                                                       
         AR    R8,R1                                                            
         B     LSTD020                                                          
*                                                                               
LSTD040  MVC   LOGLDAT,SPACES                                                   
         GOTO1 DATCON,DMCB,(1,ACLIDATE),(8,LOGLDAT)                             
         OI    LOGLDATH+6,X'80'                                                 
*&&UK*&& OI    LOGLDAT,X'F0'                                                    
         MVC   LTYPE,ACLITYPE      SAVE TYPE OF LIST.                           
         CLC   ACLIDATE,=X'FFFFFF' NEW FOR YR 2000                              
         BNE   LSTD030             NOT PERMANENT.                               
         MVC   LOGLDAT(9),=C'PERMANENT'                                         
         B     LSTD030                                                          
*                                                                               
         USING ACLDATAD,R8                                                      
LSTD060  CLI   LFSTEL,0                                                         
         BE    LSTD070                                                          
         MVI   LFSTEL,0                                                         
         MVC   WORK,SPACES                                                      
         XC    DISPERR,DISPERR     SET ERROR STORAGE.                           
         LA    R1,LOGLYN1H         GET A(LYN IN USE).                           
         ST    R1,LALYN                                                         
         LA    R1,8(R1)            GET A(THIS DISPLAY POSITION).                
         ST    R1,LADISP                                                        
         ZIC   R1,LOGLYN1H         GET L'THIS LYN.                              
         SH    R1,=H'8'                                                         
         STC   R1,LLYNLEN                                                       
*                                                                               
LSTD070  ZIC   R1,ACLDLEN          GET L'THIS ELEMENT.                          
         SH    R1,=H'7'                                                         
         STC   R1,LELLEN                                                        
         LA    R1,ACLDATA          GET A(DATA ITEM).                            
         ST    R1,LADATA                                                        
         CLI   LTYPE,C'L'                                                       
         BNE   LSTD080             IT'S AN ACCT,ME, OR W/C LIST                 
         MVI   LITEMLEN,1          SET ITEM LNTH TO L'U/L-1. IE 1.              
         B     LSTD140                                                          
*                                                                               
LSTD080  ZIC   R1,LELLEN                                                        
         SH    R1,=H'3'                                                         
         STC   R1,LELLEN                                                        
         LA    R1,ACLDACCS                                                      
         ST    R1,LADATA                                                        
         MVC   LUNIT(3),ACLDAUNT   SAVE NEW UNIT/LEDGER/LEVEL.                  
         MVC   WORK(3),=C'UL='     SET TO DISPLAY UNIT/LEDGER.                  
         MVC   WORK+3(2),LUNIT                                                  
         MVI   LITEMLEN,5          DUMMY LENGTH FOR UL=AA.                      
         BAS   RE,ITEMOUT          DISPLAY THEM.                                
         BAS   RE,GETUNIT          VALIDATE UNIT.                               
         BE    *+8                 IT'S ALRIGHT.                                
         BAS   RE,SETERROR         NO GOOD, STORE THE ERROR.                    
         BAS   RE,GETLEDG          VALIDATE THE LEDGER.                         
         BE    *+8                 IT'S ALRIGHT.                                
         BAS   RE,SETERROR         NO GOOD, STORE THE ERROR.                    
         BAS   RE,GETLVL           VALIDATE THE LEVEL.                          
*                                                                               
LSTD100  MVC   WORK(4),=C'LVL='    SET TO DISPLAY LEVEL.                        
         MVC   WORK+4(1),LLEVEL                                                 
         OI    WORK+4,X'F0'        EBCDICIZE THE LEVEL NUMBER.                  
         MVC   DUB+1(1),LITEMLEN   SAVE REAL ITEM LENGTH.                       
         MVI   LITEMLEN,5          DUMMY ITEM LENGTH FOR LVL=N.                 
         BAS   RE,ITEMOUT          DISPLAY IT.                                  
         ZIC   R1,ACLDITLN                                                      
         BCTR  R1,0                                                             
         STC   R1,LITEMLEN                                                      
         CLI   ERROR,NOLVLLDG      WAS THERE A LEVEL ERROR.                     
         BNE   *+8                 NO.                                          
         BAS   RE,SETERROR         YES, HOLD IT FOR LATER.                      
*                                                                               
LSTD140  ZIC   R1,LITEMLEN         R1 = L'DATA ITEM.                            
         L     RE,LADATA           RE = A(DATA ITEM).                           
         EX    R1,GETITEM                                                       
         LA    R1,1(R1)                                                         
         LA    RE,0(R1,RE)         RE = A(NEXT DATA ITEM).                      
         ST    RE,LADATA                                                        
         ZIC   RE,LELLEN                                                        
         SR    RE,R1                                                            
         STC   RE,LELLEN           RE = REMAINING L'ELEMENT.                    
*                                                                               
         CLI   LTYPE,C'L'                                                       
         BE    LSTD160                                                          
         BAS   RE,GETACC                                                        
         BE    *+8                                                              
         BAS   RE,SETERROR                                                      
         B     LSTD180                                                          
*                                                                               
LSTD160  MVC   LUNIT(2),WORK                                                    
         MVI   LLEVEL,1            DUMMY LEVEL FOR LEDGER LISTS.                
         BAS   RE,ITEMOUT                                                       
         BAS   RE,GETUNIT                                                       
         BE    *+8                 UNIT IS OK.                                  
         BAS   RE,SETERROR                                                      
         BAS   RE,GETLEDG                                                       
         BE    *+8                 LEDGER IS OK.                                
         BAS   RE,SETERROR                                                      
         B     LSTD190                                                          
*                                                                               
LSTD180  BAS   RE,ITEMOUT                                                       
LSTD190  CLI   LELLEN,0                                                         
         BNE   LSTD140             ELEMENT CONTINUES.                           
         B     LSTD030             ELEMENT ENDS, GET NEXT.                      
*                                                                               
LSTDEND  LA    R2,LOGLNAMH                                                      
         OC    DISPERR,DISPERR                                                  
         BZ    EXIT2               NOT HOLDING ANY ERRORS.                      
         L     R2,DISPERR          GET A(ERROR LYN).                            
         MVC   ERROR,DISPERN       GET ERROR MESSAGE NUMBER.                    
         B     EXIT2                                                            
*                                                                               
GETITEM  MVC   WORK(0),0(RE)                                                    
DISPITEM MVC   0(0,RF),WORK                                                     
         SPACE 1                                                                
ITEMOUT  NTR1                                                                   
         ZIC   R1,LITEMLEN                                                      
         LA    RE,WORK(R1)         RE = A(LAST BYTE OF MAX LNTH ITEM).          
         LA    R1,1(R1)            R1 = MAX REAL L'ITEM                         
         CLI   0(RE),C' '          FIND L'ITEM LESS TRAILING SPACES.            
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                ITEM WAS ONLY SPACES.                        
*                                                                               
         STC   R1,DUB              SAVE REAL L'INPUT ITEM.                      
         LA    R1,1(R1)            R1 = L'ITEM + PRECEDING COMMA.               
         CLM   R1,1,LLYNLEN                                                     
         BH    IOUT100             ITEM + COMMA TOO LONG FOR LYN.               
         L     RF,LADISP           ITEM + COMMA FITS ON LYN.                    
         L     RE,LALYN            FOR FIRST ITEM IN LYN,                       
         LA    RE,8(RE)            DON'T INSERT PRECEDING COMMA.                
         CR    RF,RE                                                            
         BE    IOUT120                                                          
         MVI   0(RF),C','          SET THE COMMA.                               
         LA    RF,1(RF)                                                         
         ST    RF,LADISP           SET DISPLACEMENT PAST COMMA.                 
         B     IOUT120                                                          
*                                                                               
IOUT100  BAS   RE,NEXTLYN          FIND A(START OF NEXT LYN, ETC.).             
         ZICM  RE,LALYN,4                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,0(RE)                                                         
         SH    R1,=H'8'            R1 = L'DATA PORTION OF LYN.                  
         STC   R1,LLYNLEN                                                       
*                                                                               
IOUT120  L     RF,LADISP           RF = A(SLOT FOR ITEM).                       
         ZIC   R1,DUB              R1 = SAVED L'ITEM.                           
         BCTR  R1,0                                                             
         EX    R1,DISPITEM         MVC   0(0,RF),WORK                           
         LA    R1,1(R1)            R1 = REAL L'ITEM                             
         LA    RF,0(R1,RF)                                                      
         ST    RF,LADISP           SET A(NEXT SLOT IN LYN).                     
         ZIC   RF,LLYNLEN                                                       
         SR    RF,R1                                                            
         BCTR  RF,0                RF V REMAINING L'LYN.                        
         STC   RF,LLYNLEN                                                       
         MVC   WORK,SPACES                                                      
         B     OKXIT                                                            
*                                                                               
SETERROR OC    DISPERR,DISPERR                                                  
         BNZR  RE                  HOLD ONLY FIRST ERROR ON DISPLAY.            
         L     R2,LALYN                                                         
         ST    R2,DISPERR          SAVE A(ERROR LYN).                           
         MVC   DISPERN,ERROR       SAVE ERROR MESSAGE NUMBER.                   
         BR    RE                                                               
         EJECT                                                                  
NEXTLYN  NTR1                                                                   
         L     RE,LALYN            RE = A(PRESENT LYN).                         
         ZIC   RF,0(RE)            RF = L'LYN + L'HEADER.                       
         AR    RE,RF               RE = A(PUTATIVE NEXT LINE).                  
         LA    RF,LOGTAB2H                                                      
         CR    RE,RF                                                            
         BNL   NXLYN020            IT ISN'T A LINE AT ALL.                      
         ST    RE,LALYN                                                         
         ZIC   RF,5(RE)            RF = L'DATA IN LYN.                          
         STC   RF,LLYNLEN                                                       
         LA    RE,8(RE)            RE = A(1ST DATA IN LYN).                     
         ST    RE,LADISP                                                        
         B     OKXIT                                                            
*                                                                               
NXLYN020 XC    LALYN,LALYN         PASS END OF SCREEN.                          
         B     OKXIT                                                            
         EJECT                                                                  
*              BUILD THE LIST RECORD.                                           
*                                                                               
LSTBUILD LA    R2,LOGLNAMH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(7),=C'DELETE '                                              
         BNE   LSTB020                                                          
         USING ACKEYD,R8                                                        
         LA    R8,IO2                                                           
         OI    ACSTATUS,X'80'      DELETE THE RECORD                            
         B     EXIT2                                                            
         DROP  R8                                                               
*                                                                               
LSTB020  GOTO1 NAMIN               FIRST, THE NAME ELEMENT.                     
         XC    ELEMENT,ELEMENT                                                  
         LA    R8,ELEMENT          THEN THE LIST TYPE ELEMENT.                  
         USING ACLISTD,R8                                                       
         MVI   ACLIEL,ACLIELEQ                                                  
         MVI   ACLILEN,ACLILNEQ                                                 
         MVI   ITEMS,0             SET NO DATA I/P MARKER.                      
         MVI   ACLITYPE,C'L'                                                    
         CLC   LOGLYN1(3),=C'UL='                                               
         BNE   LSTB040             MUST BE LEDGER TYPE                          
         MVI   ACLITYPE,C'M'                                                    
         CLC   LOGLYN1+3(2),=C'ME'                                              
         BE    LSTB040             MUST BE MEDIA TYPE                           
         MVI   ACLITYPE,C'W'                                                    
         CLC   LOGLYN1+6(5),=C'LVL=0'                                           
         BE    LSTB040             MUST BE WORK-CODE TYPE                       
         MVI   ACLITYPE,C'A'       ALL OTHERS ACCOUNT TYPE                      
*                                                                               
LSTB040  XC    LCONTROL,LCONTROL                                                
         MVI   LFSTEL,1                                                         
         MVC   LTYPE,ACLITYPE                                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         MVC   WORK(8),FADATE      GET TODAY'S DATE IN WORK.                    
         GOTO1 DATCON,DMCB,(4,WORK),(0,TODAY)                                   
*                                                                               
         LA    R2,LOGLDATH                                                      
         GOTO1 ANY                                                              
         BCTR  R1,0                                                             
         EX    R1,TEMPCLC          CLC   LOGLDAT(0),=C'TEMPORARY'               
         BE    LSTB080                                                          
         EX    R1,PERMCLC          CLC   LOGLDAT(0),=C'PERMANENT'               
         BE    LSTB110                                                          
         EX    R1,DELECLC          CLC   LOGLDAT(0),=C'DELETE'                  
         BNE   LSTB045                                                          
         LA    R2,LOGACTH                                                       
         CLC   LOGACT(3),=C'NEW'   IS IT ACTION NEW?                            
         BNE   LSTB095                                                          
         MVI   ERROR,DATERR        NEW/DELETE NOT VALID                         
         LA    R2,LOGLDATH                                                      
         OI    LOGLDATH+6,X'40'                                                 
         B     EXIT2                                                            
LSTB045  LA    R2,LOGLDATH                                                      
         GOTO1 DATVAL,DMCB,(0,LOGLDAT),WORK                                     
         OC    DMCB,DMCB                                                        
         BNZ   *+12                                                             
         MVI   ERROR,DATERR        BAD DATE INPUT.                              
         B     EXIT2                                                            
         CLC   WORK(6),TODAY                                                    
         BH    LSTB100             DATE IS IN FUTURE.                           
         MVI   ERROR,DATTOOLO      DATE IS IN PAST.                             
         B     EXIT2                                                            
*                                                                               
TEMPCLC  CLC   LOGLDAT(0),=C'TEMPORARY'                                         
PERMCLC  CLC   LOGLDAT(0),=C'PERMANENT'                                         
DELECLC  CLC   LOGLDAT(0),=C'DELETE'                                            
*                                                                               
LSTB080  GOTO1 GETDAY,DMCB,TODAY,WORK   FIND DAY TO GIVE RETENTION              
         LA    R2,3                     OF THREE WEEKDAYS.                      
         CLI   0(R1),3                                                          
         BL    *+8                                                              
         LA    R2,5                                                             
         GOTO1 ADDAY,DMCB,TODAY,WORK,(R2)                                       
         B     LSTB100                                                          
*                                                                               
LSTB095  DS    0H                                                               
         USING ACKEYD,R8                                                        
         LA    R8,IO2                                                           
         OI    ACSTATUS,X'80'      DELETE THE RECORD                            
         B     EXIT2                                                            
         DROP  R8                                                               
*                                                                               
LSTB100  DS    0H                                                               
         USING ACLISTD,R8                                                       
         GOTO1 DATCON,DMCB,WORK,(1,ACLIDATE)                                    
         B     *+10                                                             
LSTB110  MVC   ACLIDATE,=X'FFFFFF'                                              
         GOTO1 ADDANEL                                                          
*                                                                               
         USING ACLDATAD,R8                                                      
         LA    R8,ELEMENT                                                       
         MVI   ELEMENT,0                                                        
         BAS   RE,PUTDEL           SET UP ELEMENT                               
         MVC   LUNIT(3),SPACES                                                  
         MVI   GOODEL,C'N'                                                      
         MVI   ERROR,MISSING                                                    
         LA    R2,LOGLYN1H         R2 = A(1ST LIST DATA LYN).                   
         CLI   5(R2),0                                                          
         BE    EXIT2               NOTHING ON FIRST LINE                        
TOP      GOTO1 SCANNER,DMCB,(R2),(40,SCANBLK)                                   
         CLI   DMCB+4,0                                                         
         BNE   TOP2                                                             
         MVI   ERROR,INVALID       BAD INPUT LINE                               
         B     EXIT2                                                            
         SPACE 1                                                                
TOP2     MVC   LINCOUNT,DMCB+4     SAVE # OF ITEMS IN SCANBLK LINE              
         BAS   RE,CHKFORM          CHECK FOR CORRECT FORMAT                     
         BE    *+12                                                             
         MVI   ERROR,ERRFORM                                                    
         B     EXIT2                                                            
         ZIC   R3,LINCOUNT         NUMBER OF ITEMS IN SCANBLK                   
         LA    R4,SCANBLK                                                       
         USING SCAND,R4                                                         
TOP4     CLI   SCLEN2,0                                                         
         BE    TOPDATA             MUST BE DATA ONLY                            
         CLI   SCLEN1,2                                                         
         BNE   TOPLVL              MUST BE LVL=                                 
         CLC   SCDATA1(2),=C'UL'                                                
         BE    TOPUL                                                            
         MVI   ERROR,ULMSSNG                                                    
         B     EXIT2                                                            
         SPACE 1                                                                
TOPUL    CLI   SCLEN2,2                                                         
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT2                                                            
         CLI   GOODEL,C'Y'         DO WE ALREADY HAVE AN ELEMENT                
         BNE   TOPUL2                                                           
         ZIC   R1,LITEMLEN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ACLDITLN                                                      
         BAS   RE,PUTDEL                                                        
         MVC   LUNIT(3),SPACES                                                  
         MVI   GOODEL,C'N'                                                      
TOPUL2   MVC   LUNIT(2),SCDATA2                                                 
         BAS   RE,GETUNIT                                                       
         BNE   EXIT2                                                            
         BAS   RE,GETLEDG                                                       
         BNE   EXIT2                                                            
         L     RF,LADATA                                                        
         MVC   0(2,RF),LUNIT                                                    
         LA    RF,2(RF)                                                         
         ST    RF,LADATA                                                        
         CLC   LUNIT(2),=C'ME'                                                  
         BE    TOPMEDIA            MEDIA LIST                                   
         B     TOPX                                                             
         SPACE 1                                                                
TOPLVL   DS    0H                                                               
         CLC   LUNIT(2),SPACES                                                  
         BNE   *+12                                                             
         MVI   ERROR,ULMSSNG                                                    
         B     EXIT2                                                            
         MVI   ERROR,LVLMSSNG                                                   
         CLI   SCLEN1,3                                                         
         BNE   EXIT2                                                            
         CLC   SCDATA1(3),=C'LVL'                                               
         BNE   EXIT2                                                            
         MVI   ERROR,NOLVLLDG                                                   
         CLI   SCLEN2,1            LEVEL=1 CHAR                                 
         BNE   EXIT2                                                            
         CLI   SCBIN2+3,4          LEVEL CAN'T BE HIGHER THAN 4                 
         BH    EXIT2                                                            
         MVC   LLEVEL,SCBIN2+3                                                  
         CLC   LUNIT(2),=C'ME'                                                  
         BNE   TOPLVL2                                                          
         CLI   LLEVEL,1                                                         
         BNE   EXIT2                                                            
         B     TOPLVL4                                                          
TOPLVL2  MVI   LITEMLEN,1          WORKCODE LEN-1                               
         CLI   LLEVEL,0                                                         
         BNE   TOPLVL3                                                          
         CLI   LTYPE,C'W'          WORK-CODE                                    
         BNE   BADTYPE                                                          
         B     TOPLVL4                                                          
TOPLVL3  CLI   LTYPE,C'A'          ACCOUNT                                      
         BNE   BADTYPE                                                          
         BAS   RE,GETLVL           AND ACCOUNT LEN.                             
         BNE   EXIT2                                                            
TOPLVL4  L     RF,LADATA                                                        
         MVC   0(1,RF),LLEVEL                                                   
         LA    RF,1(RF)                                                         
         ST    RF,LADATA                                                        
         B     TOPX                                                             
         SPACE 1                                                                
TOPLEDG  DS    0H                  LEDGER                                       
         CLI   LTYPE,C'L'                                                       
         BNE   BADTYPE                                                          
         CLC   LUNIT(3),SPACES                                                  
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     EXIT2                                                            
         MVI   LITEMLEN,1                                                       
         B     TOPDATA1                                                         
TOPLEDG2 MVC   LUNIT(2),WORK                                                    
         BAS   RE,GETUNIT                                                       
         BNE   EXIT2                                                            
         BAS   RE,GETLEDG                                                       
         BNE   EXIT2                                                            
         MVC   LUNIT(2),SPACES                                                  
         B     TOPDATA3                                                         
         SPACE 1                                                                
TOPMEDIA DS    0H                  MEDIA                                        
         CLI   LTYPE,C'M'                                                       
         BNE   BADTYPE                                                          
         MVI   LITEMLEN,11                                                      
         B     TOPX                                                             
         SPACE 1                                                                
TOPDATA  DS    0H                                                               
         CLI   LUNIT,C' '                                                       
         BE    TOPLEDG             MUST BE LEDGER LIST                          
         CLI   LLEDGER,C' '                                                     
         BNE   *+12                                                             
         MVI   ERROR,ULMSSNG                                                    
         B     EXIT2                                                            
         CLI   LLEVEL,C' '                                                      
         BNE   TOPDATA1                                                         
         MVI   ERROR,INVALID                                                    
         B     EXIT2                                                            
         SPACE 1                                                                
TOPDATA1 MVC   WORK,SPACES                                                      
         ZIC   R1,LITEMLEN                                                      
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
         CR    RF,R1                                                            
         BNH   *+12                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     EXIT2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SCDATA1                                                  
         CLC   WORK,SPACES                                                      
         BE    TOPX                IGNORE - GO TO NEXT                          
         BAS   RE,GETACC                                                        
         BNE   EXIT2                                                            
TOPDATAB BAS   RE,CHKLIST          CHECK FOR DUPLICATES                         
         CLI   FOUND,C'Y'                                                       
         BNE   TOPDATAD                                                         
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'DUPCDMSG),DUPCDMSG                                     
         MVC   LOGHEAD+27(12),WORK                                              
         MVI   ERROR,X'FE'         FLAG ERROR                                   
         B     EXIT2                                                            
TOPDATAD CLI   FOUND,C'L'          ELEMENT TOO LARGE                            
         BNE   TOPDATA2                                                         
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'TOOLARGE),TOOLARGE                                     
         MVC   LOGHEAD+L'TOOLARGE(12),WORK                                      
         MVC   LOGHEAD+20(11),LOGLYN1                                           
         CLC   LOGHEAD+20(3),=C'UL='                                            
         BE    *+10                                                             
         MVC   LOGHEAD,TOOLARG2                                                 
         MVI   ERROR,X'FE'         FLAG ERROR                                   
         B     EXIT2                                                            
         SPACE 1                                                                
TOPDATA2 CLI   LTYPE,C'L'                                                       
         BE    TOPLEDG2                                                         
TOPDATA3 ZIC   R1,LITEMLEN                                                      
         L     RF,LADATA                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SCDATA1     MOVE DATA TO ELEMENT                         
         LA    RF,1(R1,RF)                                                      
         ST    RF,LADATA                                                        
         MVI   GOODEL,C'Y'                                                      
         B     TOPX                                                             
         SPACE 1                                                                
TOPX     DS    0H                                                               
         LA    R4,32(R4)           NEXT ITEM IN SCAN BLOCK                      
         BCT   R3,TOP4                                                          
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)         NEXT LINE ON SCREEN                          
         CLI   5(R2),0             IF NOTHING THERE                             
         BE    TOPEND              FINISH UP                                    
         B     TOP                                                              
         SPACE 1                                                                
TOPEND   DS    0H                                                               
         CLI   GOODEL,C'Y'                                                      
         BE    *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EXIT2                                                            
         ZIC   R1,LITEMLEN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ACLDITLN                                                      
         BAS   RE,PUTDEL                                                        
         B     OKXIT                                                            
         SPACE 2                                                                
BADTYPE  DS    0H                  INVALID TYPE CHANGE                          
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'INVTYPE),INVTYPE                                       
         MVI   ERROR,X'FE'                                                      
         B     EXIT2                                                            
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* CORRECT FORMAT IS UL=XX,LVL=N,ACCOUNT                                         
* BUMP THROUGH SCANNER LINE AND CHECK FOR:                                      
* 1) TWO U/L'S WITHOUT DATA IN BETWEEN                                          
* 2) TWO LVL='S WITHOUT DATA IN BETWEEN                                         
* 3) UL=XX,LVL=N THEN ANOTHER UL= WITHOUT ANY ACCOUNTS                          
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         USING SCAND,R4                                                         
CHKFORM  NTR1                                                                   
*                                                                               
         NI    BIT,X'FF'-LVLUSE                                                 
         NI    BIT,X'FF'-ULUSE                                                  
         LA    R2,LOGLYN1H         R2 = A(1ST LIST DATA LYN).                   
*                                                                               
         LA    R4,SCANBLK                                                       
         ZIC   R3,DMCB+4           R3=# OF ITEMS IN SCANBLK                     
CHK20    CLI   SCLEN1,3            'LVL'                                        
         BNE   CHK22                                                            
         CLC   SCDATA1(3),=C'LVL'                                               
         BNE   CHK30                                                            
         TM    BIT,LVLUSE                                                       
         BZ    *+10                                                             
         LTR   RB,RB                                                            
         B     CHKX                                                             
         OI    BIT,LVLUSE          SET BIT THAT LEVEL IS THERE                  
         NI    BIT,X'FF'-ULUSE                                                  
         B     CHKNXT                                                           
*                                                                               
CHK22    CLI   SCLEN1,2            'UL'                                         
         BNE   CHK30                                                            
         CLC   SCDATA1(2),=C'UL'                                                
         BNE   CHK32                                                            
         TM    BIT,ULUSE                                                        
         BO    CHK24                                                            
         TM    BIT,LVLUSE          PREVIOUSLY USED LVL=                         
         BO    CHK24                                                            
         OI    BIT,ULUSE           SET BIT THAT UL= IS IN USE                   
         NI    BIT,X'FF'-LVLUSE                                                 
         B     CHKNXT                                                           
CHK24    LTR   RB,RB                                                            
         B     CHKX                                                             
*                                                                               
CHK30    NI    BIT,X'FF'-LVLUSE                                                 
CHK32    NI    BIT,X'FF'-ULUSE                                                  
*                                                                               
CHKNXT   LA    R4,32(R4)           BUMP TO NEXT ITEM IN SCAN BLOCK              
         BCT   R3,CHK20                                                         
*                                                                               
CHKYES   CR    RB,RB                                                            
CHKX     B     OKXIT                                                            
*                                                                               
         EJECT                                                                  
GETLVL   NTR1                                                                   
         CLC   LUNIT(2),=C'ME'     DON'T IF MEDIA LIST                          
         BE    OKXIT                                                            
         MVI   LITEMLEN,0                                                       
         LA    RE,IO                                                            
         AH    RE,DATADISP                                                      
         USING ACHEIRD,RE                                                       
*                                                                               
GTL020   CLI   ACHREL,0                                                         
         BNE   GTL030                                                           
         MVI   ERROR,ULMSSNG                                                    
         B     GTL050                                                           
*                                                                               
GTL030   CLI   ACHREL,X'16'                                                     
         BE    GTL040              GET THE HIERARCHY ELEMENT.                   
         ZIC   RF,ACHRLEN                                                       
         AR    RE,RF                                                            
         B     GTL020                                                           
*                                                                               
GTL040   LA    RF,ACHRLEVA         RF = A(1ST LEVEL INFO).                      
         ZIC   R1,LLEVEL           R1 = NO. OF SPECIFIED ACCT LEVEL.            
         BCTR  R1,0                SET TO INDEX INTO HIER EL BY LEVEL.          
         SLL   R1,4                ENTRIES ARE 16 BYTES LONG.                   
         AR    RF,R1               RF = A(INFO FOR SPECIFIED LEVEL).            
         ZICM  R1,0(RF)            R1 = L'ACC'T FOR LEVEL.                      
         BNZ   GTL060              LEVEL IS OK.                                 
         MVI   ERROR,NOLVLLDG                                                   
*                                                                               
GTL050   LTR   RB,RB               CC OF NEQ IS ERROR.                          
         B     EXIT                                                             
*                                                                               
GTL060   BCTR  R1,0                LITEMLEN = L'ACC'T FOR LEVEL LESS 1.         
         STC   R1,LITEMLEN                                                      
         CR    RB,RB               CC OF EQ IS OK.                              
         B     OKXIT                                                            
         EJECT                                                                  
GETUNIT  NTR1                                                                   
         CLC   LUNIT(2),=C'ME'                                                  
         BE    OKXIT                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(1),LUNIT                                                   
         BAS   RE,READER                                                        
         CLI   DMCB+8,0                                                         
         BE    OKXIT                                                            
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'INUNTMSG),INUNTMSG                                     
         MVC   LOGHEAD+15(1),LUNIT                                              
         MVI   ERROR,X'FE'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
GETLEDG  NTR1                                                                   
         CLC   LUNIT(2),=C'ME'                                                  
         BE    OKXIT                                                            
         MVC   KEY,SPACES          SET TO READ NEW LEDGER.                      
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),LUNIT                                                   
         BAS   RE,READER                                                        
         CLI   DMCB+8,0                                                         
         BE    OKXIT                                                            
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'INLDGMSG),INLDGMSG                                     
         MVC   LOGHEAD+17(1),LLEDGER                                            
         MVI   ERROR,X'FE'                                                      
         B     EXIT                                                             
         EJECT                                                                  
GETACC   NTR1                                                                   
         CLI   LTYPE,C'L'          DON'T IF LEDGER OR                           
         BE    OKXIT                                                            
         CLI   LTYPE,C'M'          MEDIA LISTS                                  
         BE    OKXIT                                                            
         MVC   KEY,SPACES                                                       
         CLI   LTYPE,C'W'                                                       
         BE    GETAC5              MUST BE WORK-CODES                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),LUNIT                                                   
         ZIC   R1,LITEMLEN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),WORK       ACCT CODE.                                   
**T                                                                             
         CLI   MODE,DSPLYREC                                                    
         BNE   GETAC3                                                           
         OC    DISPERR,DISPERR                                                  
         BNZ   GETACRD                                                          
**T                                                                             
GETAC3   MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'INACTMSG),INACTMSG                                     
         LA    R1,LOGHEAD+L'INACTMSG                                            
         MVC   0(12,R1),WORK                                                    
         B     GETACRD                                                          
         SPACE 1                                                                
GETAC5   DS    0H                                                               
         CLC   WORK(2),=C'98'      DON'T BOTHER IF PLANNING                     
         BE    OKXIT                                                            
         CLC   WORK(2),=C'99'      OR BILLING W/C'S                             
         BE    OKXIT                                                            
         CLC   WORK(2),=C'**'      OR ORDERS                                    
         BE    OKXIT                                                            
         MVI   KEY,X'0A'           FIND WORK-CODE RECORD                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),LUNIT                                                   
         MVC   KEY+4(2),WORK       W-C MUST BE TWO CHARS.                       
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(L'INCDEMSG),INCDEMSG                                     
         MVC   LOGHEAD+19(2),WORK                                               
         SPACE 1                                                                
GETACRD  BAS   RE,READER                                                        
         CLI   DMCB+8,0                                                         
         BE    OKXIT               CHECK FOR PEELED/CLOSED?                     
         MVI   ERROR,X'FE'         USE MY ERROR MESSAGE                         
         B     EXIT                                                             
         EJECT                                                                  
PUTDEL   NTR1                                                                   
         CLI   ELEMENT,0                                                        
         BE    PTD040              1ST ENTRY, SET UP ONLY.                      
         L     RF,LADATA           RF = A(1ST UNUSED ELEMENT BYTE).             
         LA    RE,ELEMENT          RE = A(1ST BYTE OF ELEMENT).                 
         SR    RF,RE               RF = L'ELEMENT                               
         STC   RF,ELEMENT+1                                                     
         GOTO1 ADDANEL                                                          
PTD040   XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,ACLDELEQ                                                 
         LA    RE,ELEMENT+7                                                     
         ST    RE,LADATA           RE = A(1ST AVAILABLE DATA SLOT.              
         MVI   LFSTEL,1            SET NEW ELEMENT MARKER.                      
         XC    ENTLIST,ENTLIST     CLEAR ENTRY LIST FOR NEW ELEMENT             
         MVI   LELLEN,248          'L' TYPE DATA LENGTH = 248.                  
         CLI   LTYPE,C'L'                                                       
         BE    EXIT                IT'S A LEDGER LIST.                          
         MVI   LELLEN,245          DATA LENGTH FOR OTHERS=245.                  
         B     EXIT                                                             
         SPACE 1                                                                
READER   NTR1                                                                   
         MVC   IO(60),SPACES                                                    
         MVC   IO(L'KEY),KEY                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMREAD'),=C'ACCOUNT',IO,IO,    *        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CHECK FOR DUPLICATE ENTRIES                           
         SPACE 2                                                                
CHKLIST  NTR1                                                                   
         MVI   FOUND,C'Y'                                                       
         LA    RF,ENTLIST                                                       
         ZIC   R1,LITEMLEN         ITEM LENGTH-1                                
CHK2     CLI   0(RF),0             END OF LIST                                  
         BE    CHK4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),WORK                                                     
         BE    EXIT                ALREADY ON LIST                              
         LA    RF,1(R1,RF)         BUMP TO NEXT ITEM ON LIST                    
         B     CHK2                                                             
CHK4     LA    RE,ENTLIST+230      INSURE ELEMENT DOESN'T GET TOO BIG           
         CR    RE,RF                                                            
         BH    *+12                                                             
         MVI   FOUND,C'L'                                                       
         B     EXIT                                                             
         EX    R1,*+8              NOT FOUND, SO ADD IT                         
         B     *+10                                                             
         MVC   0(0,RF),WORK                                                     
         MVI   FOUND,C'N'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              ERROR MESSAGE CONSTANTS                                          
         SPACE 2                                                                
INUNTMSG DC    C'**ERROR** UNIT X IS INVALID'                                   
INLDGMSG DC    C'**ERROR** LEDGER X IS INVALID'                                 
INACTMSG DC    C'**ERROR** INVALID ACCOUNT = '                                  
INCDEMSG DC    C'**ERROR** WORKCODE XX IS INVALID'                              
DUPCDMSG DC    C'**ERROR** DUPLICATE CODE = '                                   
INVTYPE  DC    C'**ERROR** INCOMPATIBLE LIST TYPES'                             
TOOLARGE DC    C'TO CONTINUE, ENTER ''UL=UL,LVL=L'' BEFORE ENTRY = '            
TOOLARG2 DC    CL60'**ERROR** MAXIMUM RECORD SIZE EXCEEDED'                     
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
VTYPES   DS    0V                                                               
         DC    AL1(255)                                                         
*                                                                               
RELOC    DC    A(*)                                                             
         EJECT                                                                  
*              PROGRAM WORKING STORAGE                                          
         SPACE 2                                                                
ACCLSTD  DSECT                                                                  
WRELOC   DS    A                                                                
WVTYPES  DS    0V                                                               
GETFACT  DS    V                                                                
ADDAY    DS    V                                                                
GETDAY   DS    V                                                                
DISPERR  DS    A                                                                
DISPERN  DS    C                                                                
ITEMS    DS    C                                                                
FLAG     DS    X                                                                
ACTNEW   EQU   X'80'               ACTION=NEW                                   
TODAY    DS    CL6                 TODAY'S DATE (YYMMDD).                       
CHKSW    DS    C                                                                
GOODEL   DS    C                                                                
FOUND    DS    C                                                                
SCANNER  DS    V                                                                
ENTLIST  DS    CL256               CAN REMEMBER THIS ELEMENT ONLY               
SCANBLK  DS    CL1280              MAXIMUM 40 ENTRIES PER LINE                  
ACCLSTX  EQU   *                                                                
         SPACE 3                                                                
SCAND    DSECT                     DSECT FOR 32-BYTE SCAN BLOCK                 
SCLEN1   DS    CL1                                                              
SCLEN2   DS    CL1                                                              
SCVAL1   DS    CL1                                                              
SCVAL2   DS    CL1                                                              
SCBIN1   DS    CL4                                                              
SCBIN2   DS    CL4                                                              
SCDATA1  DS    CL10                                                             
SCDATA2  DS    CL10                                                             
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME7D                                                       
*                                                                               
LCONTROL DS    0CL27                                                            
LALYN    DS    A                                                                
LADISP   DS    A                                                                
LADATA   DS    A                                                                
         DS    2A                                                               
LUNIT    DS    C                                                                
LLEDGER  DS    C                                                                
LLEVEL   DS    C                                                                
LTYPE    DS    C                                                                
LLYNLEN  DS    C                                                                
LELLEN   DS    C                                                                
LITEMLEN DS    C                                                                
*                                                                               
LFSTEL   DS    X                                                                
LINCOUNT DS    XL1                 # OF ITEMS IN SCANBLK LINE                   
BIT      DS    XL1                                                              
LVLUSE   EQU   X'80'               TO CHECK FOR DUPLICATE LVL='S                
ULUSE    EQU   X'40'               TO CHECK FOR DUPLICATE UL='S                 
*                                  AND UNIT/LEDGERS IN LIST                     
         EJECT                                                                  
* ACLFMWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
         PRINT ON                                                               
* ACLFMEQU                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052ACLFM18   05/01/02'                                      
         END                                                                    
