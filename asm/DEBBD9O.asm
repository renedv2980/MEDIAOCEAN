*          DATA SET DEBBD9O    AT LEVEL 188 AS OF 05/12/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEBBD9OA                                                                 
         TITLE 'DEMCON - BBM CONVERSION - OUTPUT PHASE'                         
DEBBD9O  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TPTWRKX-TPTWRKD,**TPTCNV                                         
         USING TPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,1           SATELLITE STATION RECORD                     
         BE    CNV6                                                             
         CLI   INTRTYP,BSCODEQU    STATION/MARKET RECORD                        
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV14                                                            
*&&DO                                                                           
         CLI   INTRTYP,UCODEQU     UNIVERSE RECORD                              
         BE    CNV12                                                            
*&&                                                                             
         CLI   INTRTYP,C'A'                                                     
         BE    CNV3                                                             
         B     CNVX                                                             
*                                                                               
* INITIALIZE PROGTAB                                                            
CNV3     DS    0H                                                               
         L     RE,VPUTBUFF                                                      
         B     CNVX                                                             
*                                                                               
CNV4     XC    SATLASTP,SATLASTP   CLEAR SATELLITE STATION TABLE                
         MVI   INTRTYP,X'FF'                                                    
         B     CNV16               GO WRITE LAST RECORD                         
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD SATELLITE STATION TABLE                                                 
*                                                                               
CNV6     LM    RE,RF,SATLASTP                                                   
         LTR   RE,RE               TEST IF FIRST TIME                           
         BNZ   *+16                                                             
         LA    RE,SATTAB           YES - SET TO START OF TABLE                  
         XC    0(L'SATTAB+1,RE),0(RE)                                           
         LR    RF,RE                                                            
*                                                                               
         CLC   INTSTA,1(RE)        TEST IF CHANGE OF PARENT                     
         BE    CNV8                                                             
         MVI   0(RF),C'P'          BUILD PARENT ENTRY                           
         MVC   1(L'INTSTA,RF),INTSTA                                            
         LR    RE,RF                                                            
         LA    RF,L'INTSTA+1(RF)                                                
*                                                                               
CNV8     MVI   0(RF),C'S'          BUILD SATELLITE ENTRY                        
         MVC   1(L'INTSAT,RF),INTSAT                                            
         LA    RF,L'INTSAT+1(RF)                                                
         MVI   0(RF),0             SET END OF TABLE                             
         STM   RE,RF,SATLASTP                                                   
         B     CNVX                                                             
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK RECORDS                   
*                                                                               
CNV10    LA    R6,THISKEY                                                       
         USING BSKEY,R6            BUILD STATION/MARKET RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   BSCODE,BSCODEQU                                                  
         MVC   BSMEDIA,MEDIA                                                    
         MVC   BSSRC,OUTSRC                                                     
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BSKMKT,INTMRKT                                                   
         MVC   BSSTYP,INTSTYP                                                   
         MVC   BSBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          MANUAL OVERRIDE OF BOOK TYPE                 
         BE    *+10                                                             
         MVC   BSBTYP,BOOKTYPE                                                  
         MVC   BSRMKT,INTMRKT                                                   
         GOTO1 ABLDREC,DMCB,(C'P',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         XC    MLKMKT,MLKMKT                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   MLKMKT,INTMRKT                                                   
         MVC   MLBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          MANUAL OVERRIDE OF BOOK TYPE                 
         BE    *+10                                                             
         MVC   MLBTYP,BOOKTYPE                                                  
         GOTO1 ABLDREC,DMCB,(C'P',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU                                                  
         MVC   SBMEDIA,MEDIA                                                    
         MVC   SBSRC,OUTSRC                                                     
         MVC   SBSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SBKMKT,INTMRKT                                                   
         MVC   SBSTYP,INTSTYP                                                   
         MVC   SBBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          MANUAL OVERRIDE OF BOOK TYPE                 
         BE    *+10                                                             
         MVC   SBBTYP,BOOKTYPE                                                  
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         B     CNVX                                                             
         EJECT                                                                  
*&&DO                                                                           
* BUILD UNIVERSE RECORDS                                                        
*                                                                               
CNV12    LA    R6,THISKEY                                                       
         USING UKEY,R6             BUILD BASIC KEY                              
         XC    THISKEY,THISKEY                                                  
         MVI   UCODE,UCODEQU                                                    
         MVC   UMEDIA,MEDIA                                                     
         MVC   USRC,OUTSRC                                                      
         MVC   UMKT,INTMRKT                                                     
         MVC   UBOOK,INTBOOK                                                    
         XC    UBOOK,=X'FFFF'                                                   
         MVC   UBTYP,INTBTYP       INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          MANUAL OVERRIDE OF BOOK TYPE                 
         BE    *+10                                                             
         MVC   UBTYP,BOOKTYPE                                                   
*                                                                               
         LA    R2,INTUNIVS         R2=A(UNIVERSE VALUES)                        
         LA    R3,1                R3=DEMO NUMBER                               
         LA    R0,22               R0=NUMBER OF VALUES                          
*                                                                               
CNV13    STC   R3,UDEMO            SET DEMO NUMBER & UNIVERSE VALUES            
         MVC   UUNIV,0(R2)                                                      
         GOTO1 ABLDREC,DMCB,(C'P',UKEY)                                         
         GOTO1 APUTTAPE                                                         
         LA    R2,4(R2)            BUMP TO NEXT VALUE                           
         LA    R3,1(R3)                                                         
         BCT   R0,CNV13            DO FOR NUMBER OF DEMOS                       
*                                                                               
         B     CNVX                                                             
         EJECT                                                                  
*&&                                                                             
* BUILD DEMO RECORDS                                                            
CNV14    DS    0H                                                               
*                                                                               
* ///  ======  BPOO                                                             
         CLI   SVINTBTY,C'W'    IF PREV RECS WERE QTR RECS                      
         BNE   CNV14A           IF NOT CONTINUE ON                              
         CLI   INTBTYP,C'P'      IF NEW REC IS PROGRAM REC                      
         BNE   CNV14A           THEN CLEAR BUFFER FOR NEW TABLE                 
         L     RE,=A(PUTBUFF)                                                   
         L     RF,=A(PUTBUFF_MAX*PROGTABL)                                      
         XCEFL                                                                  
* /// ======  BPOO                                                              
CNV14A   CLC   =C'NOT GIVEN',INTPNAM    QTR RECORDS                             
         BE    CNV14D                                                           
*                                                                               
* PUT PROGS IN TABLE                                                            
         L     R6,=A(PUTBUFF)                                                   
         USING PROGTABD,R6                                                      
*                                                                               
CNV14B   OC    0(L'PROGTAB,R6),0(R6)      LOOK FOR AN OPEN SPACE                
         BZ    CNV14C                                                           
*                                                                               
         CLC   PROGNM(L'INTPNAM),INTPNAM  IF SAME PNAME                         
         BNE   CNV14B1                                                          
         CLC   PROGSTAT,INTSTA            FROM SAME STATION                     
         BNE   CNV14B1                                                          
         CLC   PROGDAY,INTDAY             ON THE SAME DAY                       
         BNE   CNV14B1                                                          
         CLC   PROGSQH,INTSQH             WITH SAME SQH                         
         BNE   CNV14B1                                                          
         CLC   PROGEQH,INTEQH             AND EQH, THEN EXIT                    
         BE    CNVX                                                             
*                                                                               
CNV14B1  LA    R6,L'PROGTAB(R6)                                                 
         L     RE,=A(PUTBUFF)                                                   
         A     RE,=A(PUTBUFF_MAX*PROGTABL)                                      
         CR    R6,RE                                                            
         BL    CNV14B                                                           
         DC    H'0'                                                             
*                                                                               
CNV14C   MVC   PROGNM(L'INTPNAM),INTPNAM                                        
         MVC   PROGSTAT,INTSTA                                                  
         MVC   PROGDAY,INTDAY                                                   
         MVC   PROGSQH,INTSQH                                                   
         MVC   PROGEQH,INTEQH                                                   
         DROP  R6                                                               
*                                                                               
         B    CNVX       PROG RECS JUST EXIT AFTER BUILDING TABLE               
                                                                                
* QTR RECS NEED TO LOOK IN PROGTABLE FOR PROG NAMES                             
CNV14D   L     R6,=A(PUTBUFF)                                                   
         USING PROGTABD,R6                                                      
CNV14E   OC    0(L'PROGTAB,R6),0(R6)  END OF TABLE                              
         BZ    CNV14G               ASSUME SAME AS LAST QTR HOUR                
         CLC   INTSTA(4),PROGSTAT                                               
         BNE   CNV14F                                                           
         CLC   INTDAY,PROGDAY                                                   
         BNE   CNV14F                                                           
         CLC   PROGEQH,INTSQH                                                   
         BNH   CNV14F                                                           
         MVC   INTPNAM,PROGNM                                                   
         B     CNV15                                                            
*                                                                               
CNV14F   LA    R6,L'PROGTAB(R6)                                                 
         B     CNV14E                                                           
*                                                                               
CNV14G   DS    0H                                                               
*                                                                               
CNV15    DS    0H                                                               
*                                                                               
         MVC   SVINTBTY,INTBTYP   SAVE BOOKTYPE                                 
*                                                                               
         LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU                                                  
         MVC   DRMEDIA,MEDIA                                                    
         MVC   DRSRC,OUTSRC                                                     
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DRKMKT,INTMRKT                                                   
         MVC   DRSTYP,INTSTYP                                                   
         MVC   DRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         CLI   BOOKTYPE,0          MANUAL OVERRIDE OF BOOK TYPE                 
         BE    *+10                                                             
         MVC   DRBTYP,BOOKTYPE                                                  
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
         CLC   THISKEY(L'DRKMAJOR),PREVKEY                                      
         BE    CNV28                                                            
*                                  MAJOR KEY CONTROL BREAK                      
CNV16    CLI   PUTSW,C'Y'          TEST IF LAST RECORD WRITTEN                  
         BE    CNV18                                                            
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         MVC   DRHIGHD,PREVDAY     SET HIGH DAY & QTR HOUR IN KEY               
         MVC   DRHIQHR,PREVQHR                                                  
         GOTO1 APUTTAPE            PUT LAST RECORD TO TAPE                      
         MVI   PUTSW,C'Y'                                                       
*                                                                               
CNV18    XC    PREVKEY,PREVKEY     CLEAR LAST TIME VALUES                       
         XC    PREVPNAM,PREVPNAM                                                
         CLI   INTRTYP,X'FF'       EXIT IF LAST TIME HOOK                       
         BNE   CNV20                                                            
         MVI   INTRTYP,DRCODEQU                                                 
         B     CNVX                                                             
*                                  BUILD DEMO RECORDS                           
CNV20    GOTO1 ABLDREC,DMCB,THISKEY                                             
         MVC   PREVKEY,THISKEY                                                  
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         USING SATELEM,R6          BUILD SATELLITE STATION ELEMENT              
         OC    SATLASTP,SATLASTP                                                
         BZ    CNV28               EXIT IF TABLE EMPTY                          
         LA    RE,SATTAB                                                        
*                                  LOCATE PARENT IN SATELLITE TABLE             
CNV22    CLI   0(RE),0                                                          
         BE    CNV28                                                            
         CLI   0(RE),C'P'          TEST IF A PARENT ENTRY                       
         BNE   *+14                                                             
         CLC   INTSTA,1(RE)        TEST IF CORRECT PARENT                       
         BE    *+12                                                             
         LA    RE,L'INTSTA+1(RE)   NO - BUMP TO NEXT                            
         B     CNV22                                                            
*                                                                               
         LA    RE,L'INTSTA+1(RE)   RE=A(FIRST SATELLITE ENTRY)                  
         LA    R1,SATCALL          R1=A(SATELLITE IN ELEMENT)                   
         MVI   SATCODE,SATCODEQ                                                 
*                                  BUILD LIST OF SATELLITES IN ELEMENT          
CNV24    CLI   0(RE),C'S'                                                       
         BNE   CNV26                                                            
         MVC   0(L'INTSTA,R1),1(RE)                                             
         LA    R1,L'SATCALL(R1)                                                 
         LA    RE,L'INTSTA+1(RE)                                                
         B     CNV24                                                            
*                                                                               
CNV26    SR    R1,R6                                                            
         STC   R1,SATLEN           SET ELEMENT LENGTH                           
         GOTO1 APUTEL,SATELEM                                                   
*                                  BUILD QTR HOUR ELEMENT                       
CNV28    LA    R6,TEMP                                                          
         USING QHELEM,R6                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVC   QHDAY,INTDAY                                                     
         MVC   QHSQH,INTSQH                                                     
         MVC   QHEQH,INTEQH                                                     
         MVC   QHWKS,INTWEEKS                                                   
         MVC   QHPNAME(L'INTPNAM),INTPNAM                                       
         LA    R1,QHPNAME+L'INTPNAM-1                                           
         CLI   0(R1),C' '          LOCATE END OF PROGRAM NAME                   
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         SR    R1,R6                                                            
         STC   R1,QHELN            SET ELEMENT LENGTH                           
         CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV30                                                            
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
CNV30    CLI   CONDFLAG,0          TEST IF DEMO ELEMENTS BUILT                  
         BNE   CNV32                                                            
         CLI   MEDIA,C'C'          DO NOT GO TO MKTAC FOR CANADA                
         BE    CNV31                                                            
         CLI   BOOKTYPE,X'E0'      DON'T GO TO MKTACT FOR SPECIAL SPILL         
         BE    CNV31                                                            
         GOTO1 VDEMKTAC,DMCB,(C'G',DEMCOND)                                     
*                                                                               
CNV31    LA    R7,DBLOCKA                                                       
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVI   DBSELMED,C'W'                                                    
         MVC   DBSELBK,INTBOOK                                                  
*                                                                               
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDLEN,1000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CONDFLAG,1                                                       
*                                                                               
         LA    RE,XTENDTAB         RE=TABLE POINTER                             
         LA    R0,XTENDENT         R0=TABLE ENTRY COUNTER                       
         CLC   MEDIA(2),0(RE)      TEST FOR EXTENDED DATA FILE                  
         BE    *+16                YES                                          
         LA    RE,L'XTENDTAB(RE)                                                
         BCT   R0,*-14                                                          
         B     CNV32                                                            
*                                                                               
         TM    INTWEEKS,X'20'      TEST TYPICAL TIME (2WKS OR MORE)             
         BZ    *+8                 NO-DO NOT ADD DEMOS TO WEEKLY DATA           
         BAS   RE,EXTEND           ADD EXTENDED DEMO ELEMENTS                   
*                                                                               
CNV32    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV38                                                            
         LA    R1,CONDWRK1         NO - ADD DEMO ELEMENTS TO RECORD             
         SR    R0,R0                                                            
*                                                                               
CNV34    CLI   0(R1),0             TEST E-O-L                                   
         BE    CNV36                                                            
*                                                                               
         CLI   0(R1),X'5E'                                                      
         BNE   *+10                                                             
         MVC   5(2,R1),=X'6823'                                                 
*                                                                               
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV34                                                            
*                                  SAVE THIS TIME VALUES                        
CNV36    MVC   PREVDAY,INTDAY                                                   
         MVC   PREVQHR,INTEQH                                                   
         MVC   PREVPNAM,INTPNAM                                                 
         MVI   PUTSW,C'N'          SET DEMO RECORD NOT WRITTEN                  
         B     CNVX                                                             
         EJECT                                                                  
* CONDENSE QTR HOUR & DEMO ELEMENTS. PROGRAM NAME WILL BE DROPPED FROM          
* QTR HR ELEMENT IF IT MATCHES PREVIOUS. 4BYTE DEMO ELEMENTS WILL BE            
* CREATED IF ELEMENTS EXACTLY MATCH ANY PREVIOUS QTR HR ON THIS RECORD.         
*                                                                               
CNV38    LA    R1,CONDWRK2         R1=A(OUTPUT,STRING)                          
         LA    R6,TEMP                                                          
         USING QHELEM,R6           R6=A(QTR HOUR ELEMENT)                       
         SR    RE,RE                                                            
         IC    RE,QHELN            GET LENGTH (INCLUDING PROGRAM NAME)          
         CLC   INTPNAM,PREVPNAM    TEST IF PROGRAM NAME MATCHES PREV.           
         BNE   *+8                                                              
         LA    RE,QHPNAME-QHELEM   GET LENGTH (EXCLUDING PROGRAM NAME)          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),QHELEM      MOVE ELEMENT TO OUTPUT STRING                
         STC   RE,1(R1)            SET NEW ELEMENT LENGTH                       
         AR    R1,RE                                                            
         MVI   0(R1),0                                                          
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         ST    R6,AREC                                                          
         MVC   RECLEN,DRRLEN       SAVE CURRENT OUTPUT RECORD LENGTH            
         LA    R6,DRFRSTEL                                                      
         ST    R6,AFRSTEL          SAVE A(FIRST ELEMENT)                        
         LA    R2,CONDWRK1         R2=A(INPUT STRING)                           
*                                  CONDENSE DEMO ELEMENTS                       
CNV40    CLI   0(R2),0             TEST E-O-L                                   
         BE    CNV48                                                            
         L     R6,AFRSTEL          R6=A(FIRST ELEMENT)                          
*                                  FIND MATCH WITH PREVIOUS ELEMENT             
CNV42    CLI   0(R6),0             TEST E-O-R                                   
         BE    CNV44                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                GET L'ELEMENT-1                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R2)       MATCH RECORD EL WITH INPUT EL                
         BE    *+12                                                             
         LA    R6,1(RE,R6)         NOT EQUAL - BUMP TO NEXT                     
         B     CNV42                                                            
*                                  BUILD DUPLICATE DEMO ELEMENT                 
         MVC   0(1,R1),0(R2)                                                    
         MVI   1(R1),4                                                          
         S     R6,AREC                                                          
         STCM  R6,3,2(R1)          SET DISP INTO RECORD OF DUPLICATE            
         OI    2(R1),X'80'                                                      
         LA    R1,4(R1)                                                         
         B     CNV46                                                            
*                                  MOVE INPUT EL TO OUTPUT STRING               
CNV44    IC    RE,1(R2)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)                                                    
         AR    R1,RE                                                            
*                                  BUMP TO NEXT INPUT ELEMENT                   
CNV46    MVI   0(R1),0                                                          
         IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     CNV40                                                            
*                                  TEST IF CONDENSED STRING WILL FIT            
CNV48    LA    RE,CONDWRK2                                                      
         SR    R1,RE                                                            
         AH    R1,RECLEN                                                        
         CH    R1,MAXLEN                                                        
         BNH   CNV50                                                            
*                                  NO - WRITE PREVIOUS DEMO RECORD              
         MVI   PUTSW,C'N'                                                       
         B     CNV16                                                            
*                                  YES - ADD CONDENSED ELS TO RECORD            
CNV50    LA    R1,CONDWRK2                                                      
         SR    R0,R0                                                            
         B     CNV34                                                            
         EJECT                                                                  
* SUB-ROUTINE TO ADD EXTENDED DATA TO TIME PERIOD DEMO ELEMENTS                 
* FOR A QUARTER-HOUR                                                            
*                                                                               
* AT ENTRY, CONDWRK1 CONTAINS THE DEMO ELEMENTS RETURNED BY DEMEL               
* ON EXIT, CONDWRK1 HAS FULL SET OF ELEMENTS INCLUDING EXTENDED DATA            
*                                                                               
EXTEND   NTR1                                                                   
         LA    R0,CONDWRK2         FIRST CLEAR CONDWRK2                         
         LA    R1,1000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   CONDWRK2,C'R'       SET UP DUMMY TIME PERIOD KEY                 
         MVC   CONDWRK2+20(2),=Y(DRFRSTEL-DRKEY+1)                              
*                                                                               
         LA    R3,CONDWRK1         R3=POINTER TO DEMO ELEMENTS                  
         SR    R0,R0                                                            
EXT1     CLI   0(R3),0             TEST FOR E-O-R                               
         BE    EXT2                YES                                          
         GOTO1 CHELLO,DMCB,(C'P',DEMFIL),CONDWRK2,(R3)                          
         IC    R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         B     EXT1                                                             
*                                                                               
EXT2     LA    R7,DBLOCKA          INITIALIZE DBLOCK FOR RE-CALCULATE           
         USING DBLOCKD,R7          ON TIME PERIOD RECORD                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         ST    RA,DBCOMFCS                                                      
         LA    R1,CONDWRK2                                                      
         ST    R1,DBAREC                                                        
         LA    R1,DRFRSTEL-DRKEY(R1)                                            
         ST    R1,DBAQUART                                                      
*                                                                               
         XC    MATHFACS(MATHFACL),MATHFACS                                      
         ST    R7,MATHABLK         SET UP DEMOMATH BLOCK                        
         MVC   MATHIFIL,DBFILE                                                  
         MVC   MATHOFIL,DBFILE                                                  
         MVC   MATHOSRC(1),DBSELSRC                                             
*                                                                               
         GOTO1 CDEMOMTH,DMCB,=C'REC',DBAREC,DBAREC,MATHFACS                     
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXT4     L     R3,DBAREC           R3=POINTS TO RECORD WITH COMPLETE            
         LA    R0,CONDWRK1         SET OF ELEMENTS                              
         LA    R1,1000                                                          
         LA    RE,DRFRSTEL-DRKEY(R3) POINT TO FIRST ELEMENT                     
         SR    RF,RF                                                            
         ICM   RF,3,DRRLEN-DRKEY(R3) GET RECORD LENGTH                          
         SH    RF,=Y(DRFRSTEL-DRKEY+1) LESS 1ST EL DISP AND EOR MARKER          
         STCM  RF,3,CONDLEN        SAVE LENGTH OF ELEMENTS                      
         MVCL  R0,RE               EXTRACT ELEMENTS/CLEAR REST OF AREA          
*                                                                               
EXTENDX  B     CNVX                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* TABLE OF EXTENDED DATA FILES - BYTES 0-1 = MEDIA/SOURCE                       
*                                                                               
XTENDTAB DS    0CL2                                                             
         DC    C'TN'               US NSI                                       
XTENDENT EQU   (*-XTENDTAB)/L'XTENDTAB                                          
*                                                                               
SATLASTP DC    A(0)                A(LAST PARENT IN SATTAB)                     
SATLASTS DC    A(0)                A(LAST SATELLITE IN SATTAB)                  
SATTAB   DS    50CL5               PARENT/SATELLITE STATION TABLE               
*                                  THIS TIME/LAST TIME VALUES                   
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
*                                                                               
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
MAXLEN   DC    H'1000'             MAX DEMO RECORD SIZE                         
DEMFIL   DC    C'DEMFIL'           HELLO FILE NAME                              
SVPROGNM DS    CL(L'INTPNAM)                                                    
         SPACE 2                                                                
*                                                                               
PUTBUFF_MAX EQU 100000                                                          
*                                                                               
PUTBUFF  DC    (PUTBUFF_MAX)XL(PROGTABL)'00'                                    
*                                                                               
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
TPTWRKD  DSECT                                                                  
*                                                                               
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
*                                                                               
APUTBUFF DS    A                                                                
*                                                                               
MATHFACS DS    0F                  DEMOMATH BLOCK                               
MATHABLK DS    A                   A(USER DBLOCK)                               
MATHFCTR DS    F                                                                
MATHIFIL DS    CL3                                                              
MATHOFIL DS    CL3                                                              
MATHOSRC DS    CL3                                                              
MATHFACL EQU   *-MATHFACS                                                       
* MY PROG TABLE                                                                 
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
SVINTBTY DS    CL(L'INTBTYP)                                                    
*                                                                               
TPTWRKX  EQU   *                                                                
*                                                                               
PROGTABD DSECT                                                                  
PROGTAB  DS    0CL23                                                            
PROGNM   DS    CL15                                                             
PROGSTAT DS    CL5                                                              
PROGDAY  DS    XL1                                                              
PROGSQH  DS    XL1                                                              
PROGEQH  DS    XL1                                                              
PROGTABL EQU   *-PROGTAB                                                        
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         SPACE 3                                                                
       ++INCLUDE DEINTTPTD                                                      
         SPACE 3                                                                
       ++INCLUDE DEBBCNVD                                                       
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'188DEBBD9O   05/12/17'                                      
         END                                                                    
